module Data.Vhd.Footer
    ( Footer(..)
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize
import Data.Vhd.Types
import Data.Vhd.Serialize
import Data.Vhd.UniqueId
import Data.Vhd.Time

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

footerPaddingLength :: Int
footerPaddingLength = 427
getFooterPadding :: Get ByteString
getFooterPadding = getByteString footerPaddingLength
putFooterPadding :: Put
putFooterPadding = putByteString $ B.replicate footerPaddingLength 0
