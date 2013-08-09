module Data.Vhd.Serialize where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Byteable
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Vhd.Types
import Data.Vhd.UniqueId
import Data.Vhd.Time

instance Serialize Footer where
    get = Footer
        <$> getCookie
        <*> getIsTemporaryDisk
        <*> getFormatVersion
        <*> getDataOffset
        <*> getTimeStamp
        <*> getCreatorApplication
        <*> getCreatorVersion
        <*> getCreatorHostOs
        <*> getOriginalSize
        <*> getCurrentSize
        <*> getDiskGeometry
        <*> getDiskType
        <*> getChecksum
        <*> getUniqueId
        <*> getIsSavedState
        <*  getFooterPadding
    put f = do
        putCookie             $ footerCookie             f
        putIsTemporaryDisk    $ footerIsTemporaryDisk    f
        putFormatVersion      $ footerFormatVersion      f
        putDataOffset         $ footerDataOffset         f
        putTimeStamp          $ footerTimeStamp          f
        putCreatorApplication $ footerCreatorApplication f
        putCreatorVersion     $ footerCreatorVersion     f
        putCreatorHostOs      $ footerCreatorHostOs      f
        putOriginalSize       $ footerOriginalSize       f
        putCurrentSize        $ footerCurrentSize        f
        putDiskGeometry       $ footerDiskGeometry       f
        putDiskType           $ footerDiskType           f
        putChecksum           $ footerChecksum           f
        putUniqueId           $ footerUniqueId           f
        putIsSavedState       $ footerIsSavedState       f
        putFooterPadding

instance Serialize BatmapHeader where
    get = BatmapHeader
        <$> getCookie
        <*> getDataOffset
        <*> getWord32be
        <*> getVersion
        <*> getChecksum
    put b = do
        putCookie     $ batmapHeaderCookie   b
        putDataOffset $ batmapHeaderOffset   b
        putWord32be   $ batmapHeaderSize     b
        putVersion    $ batmapHeaderVersion  b
        putChecksum   $ batmapHeaderChecksum b

footerPaddingLength = 427
getFooterPadding = getByteString footerPaddingLength
putFooterPadding = putByteString $ B.replicate footerPaddingLength 0

headerPaddingLength = 256
getHeaderPadding = skip headerPaddingLength
putHeaderPadding = putByteString $ B.replicate headerPaddingLength 0

getCookie = cookie <$> getByteString 8
putCookie (Cookie c) = putByteString c

getBlockSize       = BlockSize <$> getWord32be
putBlockSize (BlockSize sz) = putWord32be sz
getChecksum        = getWord32be
putChecksum        = putWord32be
getCurrentSize     = getWord64be
putCurrentSize     = putWord64be
getDataOffset      = getWord64be
putDataOffset      = putWord64be
getMaxTableEntries = getWord32be
putMaxTableEntries = putWord32be
getOriginalSize    = getWord64be
putOriginalSize    = putWord64be
getTableOffset     = getWord64be
putTableOffset     = putWord64be

getParentTimeStamp = getTimeStamp
putParentTimeStamp = putTimeStamp

getTimeStamp                  = VhdDiffTime <$> getWord32be
putTimeStamp (VhdDiffTime ts) = putWord32be ts

getCreatorApplication = creatorApplication <$> getByteString 4
putCreatorApplication (CreatorApplication c) = putByteString c

getCreatorHostOs = convert <$> getWord32be where
    convert 0x4D616320 = CreatorHostOsMacintosh
    convert 0x5769326B = CreatorHostOsWindows
    convert _          = CreatorHostOsUnknown
putCreatorHostOs v = putWord32be $ case v of
    CreatorHostOsMacintosh -> 0x4D616320
    CreatorHostOsWindows   -> 0x5769326B
    CreatorHostOsUnknown   -> 0

getDiskType = convert <$> getWord32be where
    convert 2 = DiskTypeFixed
    convert 3 = DiskTypeDynamic
    convert 4 = DiskTypeDifferencing
    convert _ = error "invalid disk type"
putDiskType v = putWord32be $ case v of
    DiskTypeFixed        -> 2
    DiskTypeDynamic      -> 3
    DiskTypeDifferencing -> 4

getDiskGeometry = DiskGeometry <$> getWord16be <*> getWord8 <*> getWord8
putDiskGeometry (DiskGeometry c h s) = putWord16be c >> putWord8 h >> putWord8 s

getIsTemporaryDisk = (\n -> n .&. 1 == 1) <$> getWord32be
putIsTemporaryDisk i = putWord32be ((if i then 1 else 0) .|. 0x2)

getIsSavedState = (== 1) <$> getWord8
putIsSavedState i = putWord8 (if i then 1 else 0)

getUniqueId = uniqueId <$> getByteString 16
putUniqueId uid = putByteString $ toBytes uid

getParentUniqueId :: Get UniqueId
getParentUniqueId = getUniqueId

putParentUniqueId :: UniqueId -> Put
putParentUniqueId = putUniqueId

getVersion :: Get Version
getVersion = Version <$> getWord16be <*> getWord16be
putVersion :: Version -> Put
putVersion (Version major minor) = putWord16be major >> putWord16be minor

getCreatorVersion = getVersion
putCreatorVersion = putVersion
getFormatVersion  = getVersion
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
