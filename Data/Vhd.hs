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
    , module Data.Vhd.Header
    , module Data.Vhd.Footer
    , module Data.Vhd.UniqueId
    ) where

import Control.Applicative
import Control.Monad
import Data.BitSet as BitSet
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B
import Data.Maybe
import Data.Serialize
import Data.Vhd.Bat
import Data.Vhd.Block (Block, BlockDataMapper)
import qualified Data.Vhd.Block as Block
import Data.Vhd.Checksum
import Data.Vhd.Geometry
import Data.Vhd.Node
import Data.Vhd.Types
import Data.Vhd.Header
import Data.Vhd.Footer
import Data.Vhd.UniqueId
import Data.Vhd.Utils
import Data.Vhd.Time
import Data.Vhd.Const
import Data.Vhd.Batmap
import Data.Word
import Foreign.Ptr
import Prelude hiding (subtract)
import System.FilePath.Posix
import System.IO

data Vhd = Vhd
    { vhdBlockCount :: VirtualBlockCount
    , vhdBlockSize  :: BlockSize
    , vhdNodes      :: [VhdNode]
    }

vhdSectorPerBlock :: Vhd -> Word32
vhdSectorPerBlock vhd = sz `div` Block.sectorLength
  where BlockSize sz = vhdBlockSize vhd

virtualSize :: Vhd -> VirtualByteCount
virtualSize vhd = fromIntegral (vhdBlockCount vhd) * fromIntegral bsz
  where BlockSize bsz = vhdBlockSize vhd

withVhd :: FilePath -> (Vhd -> IO a) -> IO a
withVhd = withVhdInner []
  where

    blockCount node = headerMaxTableEntries $ nodeHeader node
    blockSize  node = headerBlockSize       $ nodeHeader node
    diskType   node = footerDiskType        $ nodeFooter node

    withVhdInner accumulatedNodes filePath f =
        withVhdNode filePath $ \node -> do
            if diskType node == DiskTypeDifferencing
                then withVhdInner (node : accumulatedNodes) (parentPath node) f
                else do
                    validateBlockSize $ blockSize node
                    f $ Vhd
                        -- TODO: require consistent block count and size across all nodes.
                        { vhdBlockCount = blockCount node
                        , vhdBlockSize  = blockSize  node
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
validateBlockSize :: BlockSize -> IO ()
validateBlockSize (BlockSize value)
    | integerValue > integerLimit =
        error "Cannot open VHD file with block size greater than upper bound of platform integer."
    | otherwise =
        return ()
    where
        integerValue = fromIntegral (value          ) :: Integer
        integerLimit = fromIntegral (maxBound :: Int) :: Integer

data CreateParameters = CreateParameters
    { createBlockSize         :: BlockSize
    , createDiskType          :: DiskType
    , createParentTimeStamp   :: Maybe VhdDiffTime
    , createParentUnicodeName :: Maybe ParentUnicodeName
    , createParentUniqueId    :: Maybe UniqueId
    , createTimeStamp         :: Maybe VhdDiffTime
    , createUuid              :: Maybe UniqueId
    , createUseBatmap         :: Bool
    , createVirtualSize       :: VirtualByteCount
    } deriving (Show, Eq)

defaultCreateParameters :: CreateParameters
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
        uniqueid    <- randomUniqueId
        create' filePath $ createParams
            { createTimeStamp = Just $ maybe nowVhdEpoch id $ createTimeStamp createParams
            , createUuid      = Just $ maybe uniqueid    id $ createUuid      createParams
            }

create' :: FilePath -> CreateParameters -> IO ()
create' filePath createParams =

    withFile filePath WriteMode $ \handle -> do
        B.hPut handle $ encode footer
        B.hPut handle $ encode header
        hAlign handle sectorLength
        -- create a BAT with every entry initialized to 0xffffffff.
        B.hPut handle $ B.replicate (fromIntegral batSize) 0xff
        -- maybe create a batmap
        when (createUseBatmap createParams) $ do
            hAlign handle sectorLength
            headerPos <- hTell handle
            B.hPut handle $ encode $ batmapHeader headerPos
            hAlign handle sectorLength
            B.hPut handle $ B.replicate (fromIntegral (maxTableEntries `div` 8)) 0x0
        hAlign handle sectorLength
        B.hPut handle $ encode footer

    where
        BlockSize bsz   = createBlockSize createParams
        virtSize        = createVirtualSize createParams

        maxTableEntries = fromIntegral (virtSize `divRoundUp` fromIntegral bsz)
        batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
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
            , footerOriginalSize       = virtSize
            , footerCurrentSize        = virtSize
            , footerDiskGeometry       = diskGeometry (virtSize `div` Block.sectorLength)
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

        batmapHeader headerPos = BatmapHeader
            { batmapHeaderCookie   = cookie "tdbatmap"
            , batmapHeaderOffset   = fromIntegral (headerPos + sectorLength)
            , batmapHeaderSize     = (maxTableEntries `div` 8) `divRoundUp` sectorLength
            , batmapHeaderVersion  = Version 1 2
            , batmapHeaderChecksum = 0
            , batmapHeaderMarker   = 0
            , batmapHeaderKeyHash  = KeyHash Nothing
            , batmapHeaderReserved = B.replicate 418 0
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
        , createUseBatmap         = hasBatmap headNodeBat
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
--
-- TODO: modify this function to read sub-blocks where appropriate.
readDataRange :: Vhd     -- ^ Vhd chain to read from
              -> Word64  -- ^ offset address in the VHD
              -> Word64  -- ^ number of byte to read
              -> IO BL.ByteString
readDataRange vhd offset len
    | offset + len > virtualSize vhd = error "cannot read data past end of VHD."
    | otherwise = trim . BL.fromChunks <$> mapM (readDataBlock vhd) [blockFirst..blockLast]
  where
        (blockFirst,BlockByteAddress toDrop,_) = vaddrToBlock startAddr (vhdBlockSize vhd)
        (blockLast,_,_)       = vaddrToBlock endAddr (vhdBlockSize vhd)
        trim       = BL.take (fromIntegral len) . BL.drop (fromIntegral toDrop)
        startAddr = VirtualByteAddress offset
        endAddr   = VirtualByteAddress (offset + len)

-- | Writes data to the given virtual address of the given VHD.
writeDataRange :: Vhd           -- ^ Vhd chain to write to
               -> Word64        -- ^ offset address in the VHD
               -> BL.ByteString -- ^ the data to write in the VHD
               -> IO ()
writeDataRange vhd iniOffset iniContent = write (VirtualByteAddress iniOffset) iniContent
  where
    write :: VirtualByteAddress -> BL.ByteString -> IO ()
    write offset content
        | offset > VirtualByteAddress offsetMax = error "cannot write data past end of VHD."
        | BL.null content        = return ()
        | otherwise              = do
            sectorOffset <- lookupOrCreateBlock node blockNumber
            withMappedBlock node sectorOffset blockNumber $ \block -> do
                Block.writeDataRange bmap block blockOffset chunk
                write offsetNext contentNext
                where
                    (blockNumber, blockOffset, chunkLength)  = vaddrToBlock offset blockSize
                    chunk        = B.concat $ BL.toChunks $ fst contentSplit
                    contentSplit = BL.splitAt (fromIntegral chunkLength) content
                    contentNext  = snd contentSplit
                    offsetNext   = vaddrNextBlock offset blockSize

    (_,bmap) = getVhdBlockMapper node

    node      = head $ vhdNodes vhd
    offsetMax = virtualSize vhd
    blockSize = vhdBlockSize vhd

-- | Reads all available data from the given virtual block of the given VHD.
readDataBlock :: Vhd -> VirtualBlockAddress -> IO B.ByteString
readDataBlock vhd virtualBlockAddress =
    readDataBlockRange vhd virtualBlockAddress 0 (vhdSectorPerBlock vhd)

-- | Reads data from the given sector range of the given virtual block of the given VHD.
readDataBlockRange :: Vhd -> VirtualBlockAddress -> BlockSectorAddress -> Word32 -> IO B.ByteString
readDataBlockRange vhd virtualBlockAddress sectorOffset sectorCount =
    B.create
        (fromIntegral $ sectorCount * Block.sectorLength)
        (unsafeReadDataBlockRange vhd virtualBlockAddress sectorOffset sectorCount)

-- | Unsafely reads data from the given sector range of the given virtual block of the given VHD.
unsafeReadDataBlockRange :: Vhd
                         -> VirtualBlockAddress
                         -> BlockSectorAddress
                         -> Word32
                         -> Ptr Word8
                         -> IO ()
unsafeReadDataBlockRange vhd virtualBlockAddress sectorOffset sectorCount resultPtr = do
    _ <- B.memset resultPtr 0 $ fromIntegral $ sectorCount * sectorLength
    copySectorsFromNodes sectorsToRead =<< nodeOffsets
  where
    sectorsToRead = fromRange (fromIntegral lo) (fromIntegral hi)
      where (BlockSectorAddress lo) = sectorOffset
            hi = lo + sectorCount

    nodeOffsets :: IO [(VhdNode, PhysicalSectorAddress)]
    nodeOffsets = fmap catMaybes $ mapM maybeNodeOffset $ vhdNodes vhd
      where maybeNodeOffset node = (fmap . fmap) (node, ) $ lookupBlock (nodeBat node) virtualBlockAddress

    copySectorsFromNodes :: BitSet BlockSectorAddress -> [(VhdNode, PhysicalSectorAddress)] -> IO ()
    copySectorsFromNodes _                []                  = return ()
    copySectorsFromNodes sectorsRequested (nodeOffset : tailNodes)
        | BitSet.isEmpty sectorsRequested = return ()
        | otherwise = do sectorsMissing <- copySectorsFromNode sectorsRequested nodeOffset
                         copySectorsFromNodes sectorsMissing tailNodes

    copySectorsFromNode :: BitSet BlockSectorAddress -> (VhdNode, PhysicalSectorAddress) -> IO (BitSet BlockSectorAddress)
    copySectorsFromNode sectorsRequested (node, physicalSectorOfBlock) =
        Block.withBlock (nodeFilePath node) (vhdBlockSize vhd) 0
            physicalSectorOfBlock $ copySectorsFromNodeBlock sectorsRequested bmap
      where (bmap,_) = getVhdBlockMapper node

    copySectorsFromNodeBlock :: BitSet BlockSectorAddress -> Maybe BlockDataMapper -> Block -> IO (BitSet BlockSectorAddress)
    copySectorsFromNodeBlock sectorsRequested bmap block = do
        sectorsPresentByteString <- Block.readBitmap block
        let sectorsPresent = fromByteString sectorsPresentByteString
            sectorsMissing = sectorsRequested `subtract`  sectorsPresent
            sectorsToCopy  = sectorsRequested `intersect` sectorsPresent
        mapM_ (copySectorFromNodeBlock bmap block) (map fromIntegral $ BitSet.toList sectorsToCopy)
        return sectorsMissing

    copySectorFromNodeBlock :: Maybe BlockDataMapper -> Block -> BlockSectorAddress -> IO ()
    copySectorFromNodeBlock bmap block sectorToCopy =
        Block.unsafeReadDataRange bmap block sourceByteOffset Block.sectorLength target
      where
            sourceByteOffset = Block.blockSectorToByte sectorToCopy
            (BlockByteAddress targetByteOffset) = Block.blockSectorToByte (sectorToCopy - sectorOffset)
            target = plusPtr resultPtr $ fromIntegral $ targetByteOffset
