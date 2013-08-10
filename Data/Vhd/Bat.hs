module Data.Vhd.Bat
    ( Bat (..)
    , batGetSize
    , batmapHeaderModify
    , hasBatmap
    , containsBlock
    , lookupBlock
    , batWrite
    , batMmap
    , batIterate
    ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar
import qualified Data.ByteString.Internal as B
import Data.Byteable
import Data.Storable.Endian
import Foreign.Ptr
import Data.Serialize (encode)
import Data.Vhd.Batmap
import Data.Vhd.Types
import Data.Vhd.Header
import Data.Vhd.Const
import Data.Vhd.Utils
import System.IO.MMap

data Bat      = Bat
    { batStart  :: Ptr PhysicalSectorAddress
    , batEnd    :: Ptr PhysicalSectorAddress
    , batBatmap :: Maybe (MVar BatmapHeader)
    }

emptyEntry :: PhysicalSectorAddress
emptyEntry = 0xffffffff

hasBatmap :: Bat -> Bool
hasBatmap = maybe False (const True) . batBatmap

-- | Returns the padded size (in bytes) of the given BAT.
batGetSize :: Header -> Footer -> Int
batGetSize header _ = fromIntegral ((maxEntries * 4) `roundUpToModulo` sectorLength)
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
batWrite (Bat bptr _ _) (VirtualBlockAddress n) v = pokeBE ptr v
  where ptr = bptr `plusPtr` ((fromIntegral n) * 4)

batMmap :: FilePath -> Header -> Footer -> Maybe BatmapHeader -> (Bat -> IO a) -> IO a
batMmap file header footer batmapHeader f =
    mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, _) -> do
        m <- case batmapHeader of
                Nothing -> return Nothing
                Just bh -> Just <$> newMVar bh
        let batendPtr = ptr `plusPtr` batSize
        f $ Bat (castPtr ptr) batendPtr m
  where
        absoluteOffset = fromIntegral (headerTableOffset header)
        offsetSize     = (absoluteOffset, fromIntegral (batSize + maybe 0 sized batmapHeader + batmapSize))
        --batmapOffset   = batSize + sized (undefined :: BatmapHeader)
        batSize        = batGetSize header footer
        batmapSize     = maybe 0 (fromIntegral . (* sectorLength) . batmapHeaderSize) batmapHeader

batmapHeaderModify :: Bat -> (BatmapHeader -> BatmapHeader) -> IO ()
batmapHeaderModify bat f =
    case batBatmap bat of
        Nothing  -> return ()
        Just batmapMvar -> modifyMVar_ batmapMvar $ \batmapHdr -> do
            let newbatmapHdr = f batmapHdr
            withBytePtr (encode $ newbatmapHdr) $ \src -> B.memcpy (castPtr $ batEnd bat) src (sized batmapHdr)
            return newbatmapHdr

batIterate :: Bat -> VirtualBlockAddress -> (VirtualBlockAddress -> Maybe PhysicalSectorAddress -> IO ()) -> IO ()
batIterate bat nb f = forM_ [0 .. (nb - 1)] (\i -> lookupBlock bat i >>= \n -> f i n)
