module Data.Vhd.Batmap
    ( BatmapHeader(..)
    , KeyHash(..)
    , batmapSetKeyHash
    , batmapClearKeyHash
    ) where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Serialize
import Data.Word
import Data.Vhd.Serialize
import Data.Vhd.Types

data BatmapHeader = BatmapHeader
    { batmapHeaderCookie       :: Cookie
    , batmapHeaderOffset       :: PhysicalByteAddress
    , batmapHeaderSize         :: Word32
    , batmapHeaderVersion      :: Version
    , batmapHeaderChecksum     :: Checksum
    , batmapHeaderMarker       :: Word8
    , batmapHeaderKeyHash      :: KeyHash
    , batmapHeaderReserved     :: B.ByteString
    } deriving (Show, Eq)

newtype KeyHash = KeyHash (Maybe (B.ByteString, B.ByteString))
    deriving (Show,Eq)

instance Serialize BatmapHeader where
    get = BatmapHeader
        <$> getCookie
        <*> getDataOffset
        <*> getWord32be
        <*> getVersion
        <*> getChecksum
        <*> getWord8
        <*> get
        <*> getByteString 418
    put b = do
        putCookie     $ batmapHeaderCookie   b
        putDataOffset $ batmapHeaderOffset   b
        putWord32be   $ batmapHeaderSize     b
        putVersion    $ batmapHeaderVersion  b
        putChecksum   $ batmapHeaderChecksum b
        putWord8      $ batmapHeaderMarker   b
        put           $ batmapHeaderKeyHash  b
        putByteString $ batmapHeaderReserved b

instance Serialize KeyHash where
    get = do
        c <- getWord8
        if c /= 1
            then return $ KeyHash Nothing
            else do nonce <- getByteString 32
                    hash  <- getByteString 32
                    return $ KeyHash (Just (nonce, hash))
    put (KeyHash Nothing) = putByteString $ B.replicate (32+32+1) 0
    put (KeyHash (Just (nonce, hash))) = do
        putWord8 1
        putByteString nonce
        putByteString hash

instance Sized BatmapHeader where
    sized _ = 512

batmapClearKeyHash :: BatmapHeader -> BatmapHeader
batmapClearKeyHash bhdr = bhdr { batmapHeaderKeyHash = KeyHash Nothing }

batmapSetKeyHash :: BatmapHeader -> B.ByteString -> B.ByteString -> BatmapHeader
batmapSetKeyHash bhdr nonce hash
    | B.length nonce /= 32 || B.length hash /= 32 = error "not valid keyhash"
    | otherwise = bhdr { batmapHeaderKeyHash = KeyHash (Just (nonce, hash)) }
