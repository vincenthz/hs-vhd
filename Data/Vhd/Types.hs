{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Vhd.Types where

import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.ByteString as B
import Data.List
import qualified Data.Text as T
import Data.Vhd.Time
import Data.Vhd.Const
import Data.Vhd.UniqueId
import Data.Text.Encoding
import Data.Word
import System.Random
import Text.Printf

class Sized a where
    sized :: Num n => a -> n

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

instance Sized Footer where
    sized _ = 512

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

addrToSector :: Word64 -> PhysicalSectorAddress
addrToSector w = fromIntegral (w `div` sectorLength)

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

newtype ParentLocatorEntries = ParentLocatorEntries [ParentLocatorEntry] deriving (Show, Eq)

cookie               c = assert (B.length c ==   8) $ Cookie               c
creatorApplication   a = assert (B.length a ==   4) $ CreatorApplication   a
parentLocatorEntries e = assert (  length e ==   8) $ ParentLocatorEntries e

parentUnicodeName n
    | encodedLength > 512 = error "parent unicode name length must be <= 512 bytes"
    | otherwise           = ParentUnicodeName n
   where
        encodedLength = B.length $ encodeUtf16BE $ T.pack n
