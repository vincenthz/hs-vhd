module Data.Vhd.Header
    ( Header(..)
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Serialize
import Data.Vhd.Types
import Data.Vhd.Serialize
import Data.Vhd.UniqueId
import Data.Vhd.Time

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
    , headerReserved1            :: ByteString
    , headerParentUnicodeName    :: ParentUnicodeName
    , headerParentLocatorEntries :: ParentLocatorEntries
    } deriving (Show, Eq)

instance Serialize Header where
    get = Header
        <$> getCookie
        <*> getDataOffset
        <*> getTableOffset
        <*> getVersion
        <*> getMaxTableEntries
        <*> getBlockSize
        <*> getChecksum
        <*> getParentUniqueId
        <*> getParentTimeStamp
        <*> getByteString 4
        <*> getParentUnicodeName
        <*> get
        <*  getHeaderPadding
    put h = do
        putCookie               $ headerCookie               h
        putDataOffset           $ headerDataOffset           h
        putTableOffset          $ headerTableOffset          h
        putVersion              $ headerVersion              h
        putMaxTableEntries      $ headerMaxTableEntries      h
        putBlockSize            $ headerBlockSize            h
        putChecksum             $ headerChecksum             h
        putParentUniqueId       $ headerParentUniqueId       h
        putParentTimeStamp      $ headerParentTimeStamp      h
        putByteString           $ headerReserved1            h
        putParentUnicodeName    $ headerParentUnicodeName    h
        put $ headerParentLocatorEntries h
        putHeaderPadding

instance Sized Header where
    sized _ = 1024
