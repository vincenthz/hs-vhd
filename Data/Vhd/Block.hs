module Data.Vhd.Block
    ( Block
    , blockAddr
    , BlockDataMapper
    , sectorPerBlock
    , blockSectorToByte
    , bitmapSizeOfBlockSize
    , bitmapOfBlock
    , withBlock
    , readBitmap
    , readData
    , readDataRange
    , unsafeReadData
    , unsafeReadDataRange
    , writeDataRange
    -- * sector manipulation
    , readSector
    , writeSector
    , sectorLength
    , iterateSectors
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.Vhd.Bitmap
import Data.Vhd.Types
import Data.Vhd.Const
import Data.Vhd.Utils
import Data.Word
import Control.Applicative
import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr (newForeignPtr_)
import System.IO.MMap
import Data.Byteable

type BlockDataMapper = VirtualBlockAddress -> BlockByteAddress -> ByteString -> ByteString

data Block = Block
    { blockSize :: BlockSize           -- ^ block size in bytes
    , blockAddr :: VirtualBlockAddress -- ^ block address
    , blockPtr  :: Ptr Word8           -- ^ block data pointer
    }

newtype Data = Data (Ptr Word8)

blockSectorToByte :: BlockSectorAddress -> BlockByteAddress
blockSectorToByte (BlockSectorAddress s) = BlockByteAddress (s * sectorLength)

sectorPerBlock :: Block -> BlockSectorAddress
sectorPerBlock block = BlockSectorAddress (fromIntegral bsz `div` sectorLength)
 where BlockSize bsz = blockSize block

-- | Finds the padded size (in bytes) of the bitmap for a given block.
bitmapSizeOfBlock :: Block -> Int
bitmapSizeOfBlock block = bitmapSizeOfBlockSize $ blockSize block

-- | Finds the padded size (in bytes) of the bitmap for a given block size.
bitmapSizeOfBlockSize :: BlockSize -> Int
bitmapSizeOfBlockSize (BlockSize blocksz) = fromIntegral ((nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength)
  where nbSector = blocksz `divRoundUp` sectorLength

-- | Retrieves the bitmap for the given block.
bitmapOfBlock :: Block -> Bitmap
bitmapOfBlock block = Bitmap $ blockPtr block

-- | Retrieves the data for the given block.
dataOfBlock :: Block -> Data
dataOfBlock (Block bs _ ptr) = Data $ ptr `plusPtr` (bitmapSizeOfBlockSize bs)

-- | Obtains a direct pointer to the given data.
pointerOfData :: Data -> Ptr Word8
pointerOfData (Data ptr) = ptr

-- | Maps into memory a block of the given size, at the given file path and sector address.
withBlock :: FilePath -> BlockSize -> VirtualBlockAddress -> PhysicalSectorAddress -> (Block -> IO a) -> IO a
withBlock file blocksz@(BlockSize bsz) vba sectorOffset f =
    mmapWithFilePtr file ReadWrite (Just (offset, len)) $ \(ptr, _) ->
        f (Block blocksz vba $ castPtr ptr)
  where
        offset = (fromIntegral sectorOffset) * sectorLength
        len = (fromIntegral bsz) + (fromIntegral $ bitmapSizeOfBlockSize blocksz)

-- | Reads into memory the contents of the bitmap for the specified block.
readBitmap :: Block -> IO ByteString
readBitmap block = B.create (fromIntegral len) create
  where
        len = bitmapSizeOfBlock block
        create byteStringPtr = B.memcpy target source (fromIntegral len) where
            source = case bitmapOfBlock block of Bitmap b -> b
            target = castPtr byteStringPtr

-- | Reads all available data from the specified block.
readData :: Maybe BlockDataMapper -> Block -> IO ByteString
readData blockMapper block = readDataRange blockMapper block 0 sz
  where (BlockSize sz) = blockSize block

-- | Reads a range of data from within the specified block.
readDataRange :: Maybe BlockDataMapper -> Block -> BlockByteAddress -> Word32 -> IO ByteString
readDataRange blockDataMapper block offset len = B.create (fromIntegral len) $
    unsafeReadDataRange blockDataMapper block offset len

-- | Unsafely reads all available data from the specified block.
unsafeReadData :: Maybe BlockDataMapper -> Block -> Ptr Word8 -> IO ()
unsafeReadData blockDataMapper block =
    unsafeReadDataRange blockDataMapper block 0 (fromIntegral sz)
  where (BlockSize sz) = blockSize block

-- | Unsafely reads a range of data from within the specified block.
unsafeReadDataRange :: Maybe BlockDataMapper -- ^ an optional data mapper function
                    -> Block                 -- ^ the block
                    -> BlockByteAddress      -- ^ offset in bytes on this block
                    -> Word32                -- ^ number of bytes
                    -> Ptr Word8             -- ^ output buffer
                    -> IO ()
unsafeReadDataRange blockDataMapper block bba@(BlockByteAddress offset) len target =
    case blockDataMapper of
        Nothing   -> B.memcpy target source (fromIntegral len)
        Just bmap -> do fptr <- newForeignPtr_ source
                        let mappedSource = bmap (blockAddr block) bba $ B.fromForeignPtr fptr 0 (fromIntegral len)
                        withBytePtr mappedSource $ \src ->
                            B.memcpy target src (fromIntegral len)
  where
        source = (pointerOfData $ dataOfBlock block) `plusPtr` (fromIntegral offset)

-- | Writes data to the given byte address of the specified block.
writeDataRange :: Maybe BlockDataMapper
               -> Block
               -> BlockByteAddress
               -> ByteString
               -> IO ()
writeDataRange blockMapper block bba@(BlockByteAddress offset) content = do
    -- sectors need to be prepared for differential disk if the bitmap was clear before,
    -- at the moment assumption is it's 0ed
    bitmapSetRange bitmap (fromIntegral sectorStart) (fromIntegral sectorEnd)
    B.unsafeUseAsCString (maybe id (\bm -> bm (blockAddr block) bba) blockMapper $ content) (\source -> B.memcpy target (castPtr source) len)
  where
        len         = fromIntegral $ B.length content
        bitmap      = bitmapOfBlock block
        target      = (pointerOfData $ dataOfBlock block) `plusPtr` (fromIntegral offset)
        sectorStart = offset `div` sectorLength
        sectorEnd   = (fromIntegral offset + B.length content) `div` sectorLength

-- | Return the whole sector of a specific block if present
readSector :: Maybe BlockDataMapper -- ^ an optional data mapper function
           -> Block                 -- ^ the mapped block
           -> BlockSectorAddress    -- ^ the sector address
           -> IO (Maybe ByteString)
readSector blockMapper block (BlockSectorAddress bsa) =
    allocated >>= \isAllocated ->
        case isAllocated of
            False -> return Nothing
            True  -> Just . applyMapper <$> B.create sectorLength copy
  where
        allocated = bitmapGet bitmap (fromIntegral bsa)
        applyMapper = maybe id (\bm -> bm (blockAddr block) bba) blockMapper
        bba    = BlockByteAddress $ fromIntegral offset
        bitmap = bitmapOfBlock block
        offset = fromIntegral bsa * sectorLength
        addr   = (pointerOfData $ dataOfBlock block) `plusPtr` offset
        copy dst = B.memcpy dst addr sectorLength

-- | Write the whole sector of a specific block
--
-- the content passed need to be the size of the sector length
writeSector :: Maybe BlockDataMapper -- ^ an optional data mapper function
            -> Block                 -- ^ the mapped block
            -> BlockSectorAddress    -- ^ the sector address
            -> ByteString            -- ^ content (of sector length)
            -> IO ()
writeSector blockMapper block (BlockSectorAddress bsa) content
    | B.length content /= sectorLength = error "writeSector data need to be sector'ed size"
    | otherwise                        = do
        bitmapSet bitmap (fromIntegral bsa)
        B.unsafeUseAsCString (applyMapper content) $ \source ->
            B.memcpy target (castPtr source) sectorLength
  where
        applyMapper = maybe id (\bm -> bm (blockAddr block) bba) blockMapper
        bba    = BlockByteAddress $ fromIntegral offset
        bitmap = bitmapOfBlock block
        offset = fromIntegral bsa * sectorLength
        target = (pointerOfData $ dataOfBlock block) `plusPtr` offset

iterateSectors :: Block
               -> (BlockSectorAddress -> Bool -> IO ())
               -> IO ()
iterateSectors block f =
    forM_ [0..(nbSectors-1)] $ \sector@(BlockSectorAddress bsa) ->
        bitmapGet (bitmapOfBlock block) (fromIntegral bsa) >>= f sector
  where nbSectors = sectorPerBlock block
