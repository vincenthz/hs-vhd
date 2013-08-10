module Data.Vhd.Serialize where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Data.Byteable
import qualified Data.ByteString      as B
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Vhd.Types
import Data.Vhd.UniqueId
import Data.Vhd.Time

getCookie :: Get Cookie
getCookie = cookie <$> getByteString 8
putCookie :: Cookie -> Put
putCookie (Cookie c) = putByteString c

getBlockSize :: Get BlockSize
getBlockSize       = BlockSize <$> getWord32be
putBlockSize :: BlockSize -> Put
putBlockSize (BlockSize sz) = putWord32be sz
getChecksum :: Get Checksum
getChecksum        = getWord32be
putChecksum :: Checksum -> Put
putChecksum        = putWord32be
getCurrentSize     = getWord64be
putCurrentSize     = putWord64be
getDataOffset      = getWord64be
putDataOffset      = putWord64be
getMaxTableEntries = getWord32be
putMaxTableEntries = putWord32be
getOriginalSize    = getWord64be
putOriginalSize    = putWord64be
getTableOffset :: Get Word64
getTableOffset     = getWord64be
putTableOffset :: Word64 -> Put
putTableOffset     = putWord64be

getParentTimeStamp :: Get VhdDiffTime
getParentTimeStamp = getTimeStamp
putParentTimeStamp :: VhdDiffTime -> Put
putParentTimeStamp = putTimeStamp

getTimeStamp :: Get VhdDiffTime
getTimeStamp                  = VhdDiffTime <$> getWord32be
putTimeStamp :: VhdDiffTime -> Put
putTimeStamp (VhdDiffTime ts) = putWord32be ts

getCreatorApplication :: Get CreatorApplication
getCreatorApplication = creatorApplication <$> getByteString 4
putCreatorApplication :: CreatorApplication -> Put
putCreatorApplication (CreatorApplication c) = putByteString c

getCreatorHostOs :: Get CreatorHostOs
getCreatorHostOs = convert <$> getWord32be where
    convert 0x4D616320 = CreatorHostOsMacintosh
    convert 0x5769326B = CreatorHostOsWindows
    convert _          = CreatorHostOsUnknown
putCreatorHostOs :: CreatorHostOs -> Put
putCreatorHostOs v = putWord32be $ case v of
    CreatorHostOsMacintosh -> 0x4D616320
    CreatorHostOsWindows   -> 0x5769326B
    CreatorHostOsUnknown   -> 0

getDiskType :: Get DiskType
getDiskType = convert <$> getWord32be where
    convert 2 = DiskTypeFixed
    convert 3 = DiskTypeDynamic
    convert 4 = DiskTypeDifferencing
    convert _ = error "invalid disk type"
putDiskType :: DiskType -> Put
putDiskType v = putWord32be $ case v of
    DiskTypeFixed        -> 2
    DiskTypeDynamic      -> 3
    DiskTypeDifferencing -> 4

getDiskGeometry :: Get DiskGeometry
getDiskGeometry = DiskGeometry <$> getWord16be <*> getWord8 <*> getWord8
putDiskGeometry :: DiskGeometry -> Put
putDiskGeometry (DiskGeometry c h s) = putWord16be c >> putWord8 h >> putWord8 s

getIsTemporaryDisk :: Get Bool
getIsTemporaryDisk = (\n -> n .&. 1 == 1) <$> getWord32be
putIsTemporaryDisk :: Bool -> Put
putIsTemporaryDisk i = putWord32be ((if i then 1 else 0) .|. 0x2)

getIsSavedState :: Get Bool
getIsSavedState = (== 1) <$> getWord8
putIsSavedState :: Bool -> Put
putIsSavedState i = putWord8 (if i then 1 else 0)

getUniqueId :: Get UniqueId
getUniqueId = uniqueId <$> getByteString 16
putUniqueId :: UniqueId -> Put
putUniqueId uid = putByteString $ toBytes uid

getParentUniqueId :: Get UniqueId
getParentUniqueId = getUniqueId

putParentUniqueId :: UniqueId -> Put
putParentUniqueId = putUniqueId

getVersion :: Get Version
getVersion = Version <$> getWord16be <*> getWord16be
putVersion :: Version -> Put
putVersion (Version major minor) = putWord16be major >> putWord16be minor

getCreatorVersion :: Get Version
getCreatorVersion = getVersion
putCreatorVersion :: Version -> Put
putCreatorVersion = putVersion

getFormatVersion :: Get Version
getFormatVersion  = getVersion
putFormatVersion :: Version -> Put
putFormatVersion  = putVersion

getParentUnicodeName = parentUnicodeName . demarshall <$> getByteString 512
    where demarshall = takeWhile ((/=) '\0') . T.unpack . decodeUtf16BE
putParentUnicodeName (ParentUnicodeName c)
    | blen > 512 = error "parent unicode name length is greater than 512"
    | otherwise  = putByteString b >> putByteString (B.replicate (512 - blen) 0)
    where
        b    = encodeUtf16BE $ T.pack c
        blen = B.length b

instance Serialize ParentLocatorEntry where
    get = ParentLocatorEntry <$> getWord32be
                             <*> getWord32be
                             <*> getWord32be
                             <*> (getWord32be *> getWord64be)
    put ent = mapM_ putWord32be [locatorCode ent,locatorDataSpace ent,locatorDataLength ent,0]
           >> putWord64be (locatorDataOffset ent)

instance Serialize ParentLocatorEntries where
    get = ParentLocatorEntries <$> replicateM 8 get
    put (ParentLocatorEntries es) = mapM_ put es
