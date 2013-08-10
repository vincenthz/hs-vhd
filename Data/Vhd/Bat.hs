module Data.Vhd.Bat
    ( Bat (..)
    , batGetSize
    , hasBitmap
    , containsBlock
    , lookupBlock
    , batWrite
    , batMmap
    , batIterate
    --, batUpdateChecksum
    ) where

import Control.Monad
import Data.Bits
import Data.Storable.Endian
import Foreign.Ptr
import Foreign.Storable
import Data.Vhd.Bitmap
import Data.Vhd.Batmap
import Data.Vhd.Types
import Data.Vhd.Header
import Data.Vhd.Const
import Data.Vhd.Utils
import System.IO.MMap

data Bat      = Bat
    { batStart  :: Ptr PhysicalSectorAddress
    , batEnd    :: Ptr PhysicalSectorAddress
    , batBitmap :: Maybe BatmapHeader
    }

data Batmap   = Batmap Bitmap Int

emptyEntry :: PhysicalSectorAddress
emptyEntry = 0xffffffff

hasBitmap = maybe False (const True) . batBitmap

{-
batmapSet :: VirtualBlockAddress -> Batmap -> IO ()
batmapSet (VirtualBlockAddress n) (Batmap bitmap _) = bitmapSet bitmap (fromIntegral n)

-- FIXME batmap checksum is on the batmap header, not the actual batmap
batmapChecksum :: Batmap -> IO Checksum
batmapChecksum (Batmap (Bitmap p) sz) = complement `fmap` foldM addByte 0 [0 .. (sz - 1)]
    where addByte acc i = (p `peekElemOff` i) >>= \w -> return (acc + fromIntegral w)
-}

-- | Returns the padded size (in bytes) of the given BAT.
batGetSize :: Header -> Footer -> Int
batGetSize header footer = fromIntegral ((maxEntries * 4) `roundUpToModulo` sectorLength)
    where
        maxEntries = headerMaxTableEntries header

-- | Returns true if (and only if) the given BAT contains an entry for the
--   block at the given virtual address.
containsBlock :: Bat -> VirtualBlockAddress -> IO Bool
containsBlock bat vba = maybe False (const True) `fmap` lookupBlock bat vba

-- | Returns the physical sector address for the block at the given virtual
--   address.
lookupBlock :: Bat -> VirtualBlockAddress -> IO (Maybe PhysicalSectorAddress)
lookupBlock (Bat bptr _ _) (VirtualBlockAddress n) = justBlock `fmap` peekBE ptr
    where ptr = bptr `plusPtr` ((fromIntegral n) * 4)
          justBlock psa | psa == emptyEntry = Nothing
                        | otherwise         = Just psa

-- | Sets the physical sector address for the block at the given virtual
--   address, in the specified BAT.
batWrite :: Bat -> VirtualBlockAddress -> PhysicalSectorAddress -> IO ()
batWrite (Bat bptr _ bmap) vba@(VirtualBlockAddress n) v =
    pokeBE ptr v {->> maybe (return ()) (batmapSet vba) bmap-}
  where ptr = bptr `plusPtr` ((fromIntegral n) * 4)

batMmap :: FilePath -> Header -> Footer -> Maybe BatmapHeader -> (Bat -> IO a) -> IO a
batMmap file header footer batmapHeader f =
    mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) ->
        --let batmap    = Batmap (Bitmap (castPtr (ptr `plusPtr` batmapOffset))) batmapSize in
        let batendPtr = ptr `plusPtr` batSize in
        f $ Bat (castPtr ptr) batendPtr batmapHeader -- $ fmap (const batmap) batmapHeader
  where
        absoluteOffset = fromIntegral (headerTableOffset header)
        offsetSize     = (absoluteOffset, fromIntegral (batSize + maybe 0 sized batmapHeader + batmapSize))
        batmapOffset   = batSize + sized (undefined :: BatmapHeader)
        batSize        = batGetSize header footer
        batmapSize     = maybe 0 (fromIntegral . (* sectorLength) . batmapHeaderSize) batmapHeader

batIterate :: Bat -> VirtualBlockAddress -> (VirtualBlockAddress -> Maybe PhysicalSectorAddress -> IO ()) -> IO ()
batIterate bat nb f = forM_ [0 .. (nb - 1)] (\i -> lookupBlock bat i >>= \n -> f i n)

{-
-- | Updates the checksum in the batmap, if the batmap exists.
batUpdateChecksum :: Bat -> IO ()
batUpdateChecksum (Bat _ _        Nothing)       = return ()
batUpdateChecksum (Bat _ endptr   (Just batmap)) = do
    let batmapChecksumPtr = endptr `plusPtr` (8+8+4+4)
    checksum <- batmapChecksum batmap
    pokeBE batmapChecksumPtr checksum
-}
