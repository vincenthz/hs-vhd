{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Vhd
    ( create
    , CreateParameters (..)
    , defaultCreateParameters
    , getInfo
    , snapshot
    , readData
    , readDataRange
    , writeDataRange
    , withVhd
    , module Data.Vhd.Types
    ) where

import Control.Applicative
import Control.Monad
import Data.BitSet as BitSet
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Serialize
import Data.Time.Clock.POSIX
import Data.Vhd.Bat
import Data.Vhd.Block hiding (readData, readDataRange, writeDataRange)
import qualified Data.Vhd.Block as Block
import Data.Vhd.Checksum
import Data.Vhd.Geometry
import Data.Vhd.Node
import Data.Vhd.Types
import Data.Vhd.Utils
import Data.Vhd.Time
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Prelude hiding (subtract)
import System.FilePath.Posix
import System.IO

data Vhd = Vhd
    { vhdBlockCount :: VirtualBlockCount
    , vhdBlockSize  :: BlockByteCount
    , vhdNodes      :: [VhdNode]
    }

virtualSize :: Vhd -> VirtualByteCount
virtualSize vhd = fromIntegral (vhdBlockCount vhd) * fromIntegral (vhdBlockSize vhd)

withVhd :: FilePath -> (Vhd -> IO a) -> IO a
withVhd = withVhdInner []
  where

    blockCount node = headerMaxTableEntries $ nodeHeader node
    blockSize  node = headerBlockSize       $ nodeHeader node
    diskType   node = footerDiskType        $ nodeFooter node

    withVhdInner accumulatedNodes filePath f =
        withVhdNode filePath $ \node ->
            if diskType node == DiskTypeDifferencing
                then withVhdInner (node : accumulatedNodes) (parentPath node) f
                else f $ Vhd
                    -- TODO: require consistent block count and size across all nodes.
                    { vhdBlockCount = blockCount node
                    , vhdBlockSize  = validateBlockSize $ blockSize  node
                    , vhdNodes      = reverse $ node : accumulatedNodes
                    }
        where parentPath node = resolveColocatedFilePath filePath p
                where ParentUnicodeName p = headerParentUnicodeName $ nodeHeader node

-- The VHD specification requires an unsigned 32-bit word to encode the
-- block size of a VHD.
--
-- However, this library provides:
--     a. operations to copy data from a block into a strict ByteString.
--     b. operations to copy data from a strict ByteString into a block.
--
-- Furthermore:
--     c. for all bytestrings b:   (length of b) ≤ (maxBound :: Int).
--     d. for some systems: (maxBound :: Word32) > (maxBound :: Int).
--
-- This opens the (very remote) possibility of subtle bugs on attempting
-- to open a VHD with (block size) > (maxBound :: Int).  Therefore, this
-- function fails fast on attempting to open such a VHD file.
--
validateBlockSize :: BlockByteCount -> BlockByteCount
validateBlockSize value =
    if integerValue > integerLimit
        then error
            ( "Cannot open VHD file with block size " ++
              "greater than upper bound of platform integer." )
        else value
    where
        integerValue = fromIntegral (value          ) :: Integer
        integerLimit = fromIntegral (maxBound :: Int) :: Integer

data CreateParameters = CreateParameters
    { createBlockSize         :: BlockByteCount
    , createDiskType          :: DiskType
    , createParentTimeStamp   :: Maybe VhdDiffTime
    , createParentUnicodeName :: Maybe ParentUnicodeName
    , createParentUniqueId    :: Maybe UniqueId
    , createTimeStamp         :: Maybe VhdDiffTime
    , createUuid              :: Maybe UniqueId
    , createUseBatmap         :: Bool
    , createVirtualSize       :: VirtualByteCount
    } deriving (Show, Eq)

defaultCreateParameters = CreateParameters
    { createBlockSize         = 2 * 1024 * 1024
    , createDiskType          = DiskTypeDynamic
    , createParentTimeStamp   = Nothing
    , createParentUnicodeName = Nothing
    , createParentUniqueId    = Nothing
    , createTimeStamp         = Nothing
    , createUuid              = Nothing
    , createUseBatmap         = False
    , createVirtualSize       = 0
    }

-- | Retrieves the header and footer from a VHD file.
getInfo :: FilePath -> IO (Either String (Header, Footer))
getInfo filePath = withFile filePath ReadMode $ \handle -> do
    footer <- decode <$> B.hGet handle 512
    header <- decode <$> B.hGet handle 1024
    case (footer, header) of
        (Left err, _)      -> return $ Left err
        (_, Left err)      -> return $ Left err
        (Right f, Right h) -> return $ Right (h, f)

-- | Creates an empty VHD file with the specified parameters.
create :: FilePath -> CreateParameters -> IO ()
create filePath createParams
    | createVirtualSize createParams == 0 = error "cannot create a 0-sized VHD"
    | otherwise                           = do
        nowVhdEpoch <- getVHDTime
        uniqueid <- randomUniqueId
        create' filePath $ createParams
            { createTimeStamp = Just $ maybe nowVhdEpoch id $ createTimeStamp createParams
            , createUuid      = Just $ maybe uniqueid    id $ createUuid      createParams
            }
    where
        y2k :: Word64
        y2k = 946684800 -- seconds from the unix epoch to the vhd epoch

create' :: FilePath -> CreateParameters -> IO ()
create' filePath createParams =

    withFile filePath WriteMode $ \handle -> do
        B.hPut handle $ encode footer
        B.hPut handle $ encode header
        hAlign handle (fromIntegral sectorLength)
        -- create a BAT with every entry initialized to 0xffffffff.
        B.hPut handle $ B.replicate (fromIntegral batSize) 0xff
        -- maybe create a batmap
        when (createUseBatmap createParams) $ do
            hAlign handle (fromIntegral sectorLength)
            headerPos <- hTell handle
            B.hPut handle $ encode $ BatmapHeader
                { batmapHeaderCookie   = cookie "tdbatmap"
                , batmapHeaderOffset   = fromIntegral (headerPos + fromIntegral sectorLength)
                , batmapHeaderSize     = (maxTableEntries `div` 8) `divRoundUp` sectorLength
                , batmapHeaderVersion  = Version 1 2
                , batmapHeaderChecksum = 0xffffffff
                }
            hAlign handle (fromIntegral sectorLength)
            B.hPut handle $ B.replicate (fromIntegral (maxTableEntries `div` 8)) 0x0
        hAlign handle (fromIntegral sectorLength)
        B.hPut handle $ encode footer

    where
        virtualSize     = createVirtualSize createParams
        maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral (createBlockSize createParams))
        batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
        batPadSize      = batSize - maxTableEntries * 4
        footerSize      = 512
        headerSize      = 1024

        footer = adjustFooterChecksum $ Footer
            { footerCookie             = cookie "conectix"
            , footerIsTemporaryDisk    = False
            , footerFormatVersion      = Version 1 0
            , footerDataOffset         = footerSize
            , footerTimeStamp          = fromJust $ createTimeStamp createParams
            , footerCreatorApplication = creatorApplication "tap\0"
            , footerCreatorVersion     = if createUseBatmap createParams then Version 1 3 else Version 1 0
            , footerCreatorHostOs      = CreatorHostOsWindows
            , footerOriginalSize       = virtualSize
            , footerCurrentSize        = virtualSize
            , footerDiskGeometry       = diskGeometry (virtualSize `div` fromIntegral sectorLength)
            , footerDiskType           = createDiskType createParams
            , footerChecksum           = 0
            , footerUniqueId           = fromJust $ createUuid createParams
            , footerIsSavedState       = False
            }

        header = adjustHeaderChecksum $ Header
            { headerCookie               = cookie "cxsparse"
            , headerDataOffset           = 0xffffffffffffffff
            , headerTableOffset          = footerSize + headerSize
            , headerVersion              = Version 1 0
            , headerMaxTableEntries      = maxTableEntries
            , headerBlockSize            = createBlockSize createParams
            , headerChecksum             = 0
            , headerParentUniqueId       = fromMaybe (uniqueId $ B.replicate 16 0) (createParentUniqueId createParams)
            , headerParentTimeStamp      = fromMaybe (VhdDiffTime 0) (createParentTimeStamp createParams)
            , headerReserved1            = B.replicate 4 0
            , headerParentUnicodeName    = fromMaybe (parentUnicodeName "") (createParentUnicodeName createParams)
            , headerParentLocatorEntries = ParentLocatorEntries $ replicate 8 nullParentLocatorEntry
            }

snapshot :: Vhd -> FilePath -> IO ()
snapshot parentVhd childFilePath = do
    create childFilePath $ CreateParameters
        { createBlockSize         = vhdBlockSize parentVhd
        , createDiskType          = DiskTypeDifferencing
        , createParentTimeStamp   = Just $ footerTimeStamp   headNodeFooter
        , createParentUniqueId    = Just $ footerUniqueId    headNodeFooter
        , createParentUnicodeName = Just $ parentUnicodeName parentFilePath
        , createTimeStamp         = Nothing
        , createUuid              = Nothing
        , createUseBatmap         = hasBitmap headNodeBat
        , createVirtualSize       = virtualSize parentVhd
        }
  where headNode         = head $ vhdNodes parentVhd
        headNodeBat      = nodeBat      headNode
        headNodeFooter   = nodeFooter   headNode
        parentFilePath   = makeRelative (takeDirectory childFilePath) (nodeFilePath headNode)

-- | Reads data from the whole virtual address space of the given VHD.
readData :: Vhd -> IO BL.ByteString
readData vhd = readDataRange vhd 0 (virtualSize vhd)

-- | Reads data from the given virtual address range of the given VHD.
readDataRange :: Vhd -> VirtualByteAddress -> VirtualByteCount -> IO BL.ByteString
readDataRange vhd offset length =
    -- To do: modify this function to read sub-blocks where appropriate.
    if offset + length > virtualSize vhd
        then error "cannot read data past end of VHD."
        else fmap (trim . BL.fromChunks) (sequence blocks)
    where
        blocks     = map (readDataBlock vhd) [blockFirst .. blockLast]
        blockFirst = fromIntegral $ (offset             ) `div` blockSize
        blockLast  = fromIntegral $ (offset + length - 1) `div` blockSize
        blockSize  = fromIntegral $ vhdBlockSize vhd
        trim       = BL.take toTake . BL.drop toDrop
            where
                toTake = fromIntegral $ length
                toDrop = fromIntegral $ offset `mod` blockSize

-- | Writes data to the given virtual address of the given VHD.
writeDataRange :: Vhd -> VirtualByteAddress -> BL.ByteString -> IO ()
writeDataRange vhd offset content = write (fromIntegral offset) content
  where
    write offset content
        | offset > offsetMax = error "cannot write data past end of VHD."
        | BL.null content    = return ()
        | otherwise          = do
            sectorOffset <- lookupOrCreateBlock node (fromIntegral blockNumber)
            withBlock file (fromIntegral blockSize) sectorOffset $ \block -> do
                Block.writeDataRange block (fromIntegral blockOffset) chunk
                write offsetNext contentNext
                where
                    blockNumber  = (fromIntegral $ offset `div` blockSize) :: VirtualBlockAddress
                    blockOffset  = offset `mod` blockSize
                    chunk        = B.concat $ BL.toChunks $ fst contentSplit
                    chunkLength  = fromIntegral $ blockSize - (offset `mod` blockSize)
                    contentSplit = BL.splitAt chunkLength content
                    contentNext  = snd contentSplit
                    offsetNext   = ((offset `div` blockSize) * blockSize) + blockSize

    bat       = nodeBat node
    file      = nodeFilePath node
    node      = head $ vhdNodes vhd
    offsetMax = virtualSize vhd
    blockSize = fromIntegral $ vhdBlockSize vhd

-- | Reads all available data from the given virtual block of the given VHD.
readDataBlock :: Vhd -> VirtualBlockAddress -> IO B.ByteString
readDataBlock vhd virtualBlockAddress =
    readDataBlockRange vhd virtualBlockAddress 0 ((vhdBlockSize vhd) `div` sectorLength)

-- | Reads data from the given sector range of the given virtual block of the given VHD.
readDataBlockRange :: Vhd -> VirtualBlockAddress -> BlockSectorAddress -> BlockSectorCount -> IO B.ByteString
readDataBlockRange vhd virtualBlockAddress sectorOffset sectorCount =
    B.create
        (fromIntegral $ sectorCount * sectorLength)
        (unsafeReadDataBlockRange vhd virtualBlockAddress sectorOffset sectorCount)

-- | Unsafely reads data from the given sector range of the given virtual block of the given VHD.
unsafeReadDataBlockRange :: Vhd -> VirtualBlockAddress -> BlockSectorAddress -> BlockSectorCount -> Ptr Word8 -> IO ()
unsafeReadDataBlockRange vhd virtualBlockAddress sectorOffset sectorCount resultPtr = buildResult
  where
    buildResult :: IO ()
    buildResult = do
        B.memset resultPtr 0 $ fromIntegral $ sectorCount * sectorLength
        copySectorsFromNodes sectorsToRead =<< nodeOffsets

    sectorsToRead = fromRange lo hi
      where lo = fromIntegral $ sectorOffset
            hi = fromIntegral $ sectorOffset + sectorCount

    nodeOffsets :: IO [(VhdNode, PhysicalSectorAddress)]
    nodeOffsets = fmap catMaybes $ mapM maybeNodeOffset $ vhdNodes vhd
      where maybeNodeOffset node = (fmap . fmap) (node, ) $ lookupBlock (nodeBat node) virtualBlockAddress

    copySectorsFromNodes :: BitSet -> [(VhdNode, PhysicalSectorAddress)] -> IO ()
    copySectorsFromNodes sectorsRequested [] = return ()
    copySectorsFromNodes sectorsRequested (nodeOffset : tail)
        | BitSet.isEmpty sectorsRequested = return ()
        | otherwise = do sectorsMissing <- copySectorsFromNode sectorsRequested nodeOffset
                         copySectorsFromNodes sectorsMissing tail

    copySectorsFromNode :: BitSet -> (VhdNode, PhysicalSectorAddress) -> IO BitSet
    copySectorsFromNode sectorsRequested (node, physicalSectorOfBlock) =
        withBlock (nodeFilePath node) (vhdBlockSize vhd)
            physicalSectorOfBlock $ copySectorsFromNodeBlock sectorsRequested

    copySectorsFromNodeBlock :: BitSet -> Block -> IO BitSet
    copySectorsFromNodeBlock sectorsRequested block = do
        sectorsPresentByteString <- Block.readBitmap block
        let sectorsPresent = fromByteString sectorsPresentByteString
            sectorsMissing = sectorsRequested `subtract`  sectorsPresent
            sectorsToCopy  = sectorsRequested `intersect` sectorsPresent
        mapM_ (copySectorFromNodeBlock block) (map fromIntegral $ toList sectorsToCopy)
        return sectorsMissing

    copySectorFromNodeBlock :: Block -> BlockSectorAddress -> IO ()
    copySectorFromNodeBlock block sectorToCopy =
        unsafeReadDataRange block sourceByteOffset sectorLength target where
            sourceByteOffset = sectorLength * (sectorToCopy               )
            targetByteOffset = sectorLength * (sectorToCopy - sectorOffset)
            target = plusPtr resultPtr $ fromIntegral $ targetByteOffset
