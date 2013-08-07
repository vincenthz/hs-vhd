{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Vhd.Types where

import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.ByteString as B
import Data.List
import qualified Data.Text as T
import Data.Vhd.Time
import Data.Text.Encoding
import Data.Word
import System.Random
import Text.Printf

data Header = Header
    { headerCookie               :: Cookie
    , headerDataOffset           :: PhysicalByteAddress
    , headerTableOffset          :: PhysicalByteAddress
    , headerVersion              :: Version
    , headerMaxTableEntries      :: VirtualBlockCount
    , headerBlockSize            :: BlockSize
    , headerChecksum             :: Checksum
    , headerParentUniqueId       :: UniqueId
    , headerParentTimeStamp      :: VhdDiffTime
    , headerReserved1            :: B.ByteString
    , headerParentUnicodeName    :: ParentUnicodeName
    , headerParentLocatorEntries :: ParentLocatorEntries
    } deriving (Show, Eq)

data Footer = Footer
    { footerCookie             :: Cookie
    , footerIsTemporaryDisk    :: Bool
    , footerFormatVersion      :: Version
    , footerDataOffset         :: PhysicalByteAddress
    , footerTimeStamp          :: VhdDiffTime
    , footerCreatorApplication :: CreatorApplication
    , footerCreatorVersion     :: Version
    , footerCreatorHostOs      :: CreatorHostOs
    , footerOriginalSize       :: VirtualByteCount
    , footerCurrentSize        :: VirtualByteCount
    , footerDiskGeometry       :: DiskGeometry
    , footerDiskType           :: DiskType
    , footerChecksum           :: Checksum
    , footerUniqueId           :: UniqueId
    , footerIsSavedState       :: Bool
    } deriving (Show, Eq)

data BatmapHeader = BatmapHeader
    { batmapHeaderCookie       :: Cookie
    , batmapHeaderOffset       :: PhysicalByteAddress
    , batmapHeaderSize         :: Word32
    , batmapHeaderVersion      :: Version
    , batmapHeaderChecksum     :: Checksum
    } deriving (Show, Eq)

-- | block size
newtype BlockSize = BlockSize Word32
    deriving (Show,Eq,Ord,Num)

-- | The offset from the beginning of a block in bytes
newtype BlockByteAddress = BlockByteAddress Word32
    deriving (Show,Eq,Ord,Num)

-- | The offset from the beginning of a block in sectors
newtype BlockSectorAddress = BlockSectorAddress Word32
    deriving (Show,Eq,Ord,Num,Enum)

-- | The absolute number of the block
newtype VirtualBlockAddress = VirtualBlockAddress Word32
    deriving (Show,Eq,Ord,Num,Enum)

-- | An absolute address in byte in the vhd content space
newtype VirtualByteAddress = VirtualByteAddress Word64
    deriving (Show,Eq,Ord,Num,Enum)


type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8
type Checksum                    = Word32

type PhysicalByteAddress         = Word64
type PhysicalByteCount           = Word64
type PhysicalSectorAddress       = Word32
type PhysicalSectorCount         = Word32

type VirtualBlockCount           = Word32
type VirtualByteCount            = Word64
type VirtualSectorAddress        = Word32
type VirtualSectorCount          = Word32

vaddrPlus :: VirtualByteAddress -> Word64 -> VirtualByteAddress
vaddrPlus (VirtualByteAddress b) w = VirtualByteAddress (b + w)

vaddrToBlock :: VirtualByteAddress -> BlockSize -> (VirtualBlockAddress, BlockByteAddress, Word32)
vaddrToBlock (VirtualByteAddress b) (BlockSize blocksz) =
    (VirtualBlockAddress $ fromIntegral d, fromIntegral m, blocksz - fromIntegral m)
  where (d,m) = b `divMod` fromIntegral blocksz

-- | increment the virtual address to align to the next block
vaddrNextBlock :: VirtualByteAddress -> BlockSize -> VirtualByteAddress
vaddrNextBlock (VirtualByteAddress b) (BlockSize blocksz) =
    VirtualByteAddress (((b `div` sz) * sz) + sz)
  where sz = fromIntegral blocksz

data Version      = Version VersionMajor VersionMinor deriving (Show, Eq)
type VersionMajor = Word16
type VersionMinor = Word16

data CreatorHostOs
    = CreatorHostOsUnknown
    | CreatorHostOsWindows
    | CreatorHostOsMacintosh deriving (Show, Eq)

data DiskGeometry = DiskGeometry
    DiskGeometryCylinders
    DiskGeometryHeads
    DiskGeometrySectorsPerTrack deriving (Show, Eq)

data DiskType
    = DiskTypeFixed
    | DiskTypeDynamic
    | DiskTypeDifferencing deriving (Show, Eq)

newtype Cookie               = Cookie             B.ByteString deriving (Show, Eq)
newtype CreatorApplication   = CreatorApplication B.ByteString deriving (Show, Eq)
data ParentLocatorEntry = ParentLocatorEntry
    { locatorCode  :: Word32
    , locatorDataSpace  :: Word32
    , locatorDataLength :: Word32
    , locatorDataOffset :: Word64
    } deriving (Show, Eq)

nullParentLocatorEntry = ParentLocatorEntry 0 0 0 0

newtype ParentUnicodeName    = ParentUnicodeName  String       deriving (Show, Eq)
newtype UniqueId             = UniqueId           B.ByteString deriving (Eq)

instance Show UniqueId where
    show (UniqueId b) = intercalate "-" $ map disp [[0 .. 3], [4, 5], [6, 7], [8, 9], [10 .. 15]]
        where disp = concatMap (printf "%02x" . B.index b)

ofString :: String -> Maybe UniqueId
ofString s
    | length s /= 36 = Nothing
    | head r0 /= '-' = Nothing
    | head r1 /= '-' = Nothing
    | head r2 /= '-' = Nothing
    | head r3 /= '-' = Nothing
    | otherwise      = Just $ UniqueId $ B.pack (unhex a8 ++ unhex b4 ++ unhex c4 ++ unhex d4 ++ unhex (drop 1 r3))
  where (a8, r0) = splitAt 8 s
        (b4, r1) = splitAt 4 $ drop 1 r0
        (c4, r2) = splitAt 4 $ drop 1 r1
        (d4, r3) = splitAt 4 $ drop 1 r2

        unhex []         = []
        unhex (v1:v2:xs) = fromIntegral (digitToInt v1 * 16 + digitToInt v2) : unhex xs

instance Read UniqueId where
    readsPrec _ r = let (t,d) = splitAt 36 r
                     in case ofString t of
                            Nothing  -> []
                            Just uid -> [(uid, d)]

newtype ParentLocatorEntries = ParentLocatorEntries [ParentLocatorEntry] deriving (Show, Eq)

cookie               c = assert (B.length c ==   8) $ Cookie               c
creatorApplication   a = assert (B.length a ==   4) $ CreatorApplication   a
parentLocatorEntries e = assert (  length e ==   8) $ ParentLocatorEntries e
uniqueId             i = assert (B.length i ==  16) $ UniqueId             i

parentUnicodeName n
    | encodedLength > 512 = error "parent unicode name length must be <= 512 bytes"
    | otherwise           = ParentUnicodeName n
    where
        encodedLength = B.length $ encodeUtf16BE $ T.pack n

randomUniqueId :: IO UniqueId
randomUniqueId
    = liftM (uniqueId . B.pack)
    $ replicateM 16
    $ liftM fromIntegral
    $ randomRIO (0 :: Int, 255)
