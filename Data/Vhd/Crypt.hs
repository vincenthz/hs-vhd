module Data.Vhd.Crypt
    ( VhdCryptKey(..)
    , VhdCryptContext
    , findImplicitCryptFile
    , findImplicitCryptKey
    , vhdCryptInit
    , vhdEncrypt
    , vhdDecrypt
    ) where

import Control.Applicative ((<$>))
import Data.List (isSuffixOf)
import Data.Word
import Data.Bits (shiftR)
import Data.Vhd.Types (VirtualBlockAddress(..), BlockByteAddress(..))
import Crypto.Cipher.AES
import System.FilePath
import System.Directory
import qualified Data.ByteString as B

newtype VhdCryptKey     = VhdCryptKey B.ByteString

newtype VhdCryptContext = VhdCryptContext (AES,AES)

-- | Find implicit cryptographic key associated with a vhd node.
--
-- given a vhd node called "a.vhd" or "a", this function will looks for
-- "a,aes-xts-plain,512.key" and "a,aes-xts-plain,256.key"
--
findImplicitCryptFile :: FilePath -> IO (Maybe FilePath)
findImplicitCryptFile filepath = loop allCryptFiles
  where baseName | ".vhd" `isSuffixOf` filepath = dropExtension filepath
                 | otherwise                    = filepath
        suffixes = [ ",aes-xts-plain,512.key", ",aes-xts-plain,256.key" ]
        allCryptFiles = map (baseName ++) suffixes

        loop []     = return Nothing
        loop (f:fs) = do e <- doesFileExist f
                         if e then return (Just f) else loop fs

findImplicitCryptKey :: FilePath -> IO (Maybe VhdCryptKey)
findImplicitCryptKey filepath = do
    fpr <- findImplicitCryptFile filepath
    case fpr of
        Nothing -> return Nothing
        Just fp -> Just . VhdCryptKey <$> B.readFile fp

vhdCryptInit :: VhdCryptKey -> Maybe VhdCryptContext
vhdCryptInit (VhdCryptKey cryptKey)
    | B.length cryptKey /= 32 && B.length cryptKey /= 64 = Nothing
    | otherwise               =
        let (k1,k2) = B.splitAt ((B.length  cryptKey) `div` 2) cryptKey
         in Just $ VhdCryptContext (initAES k1, initAES k2)

-- | Encrypt using VhdCryptContext
vhdEncrypt :: VhdCryptContext -> VirtualBlockAddress -> BlockByteAddress -> B.ByteString -> B.ByteString
vhdEncrypt (VhdCryptContext cc) blockN blockOffset bs =
    encryptXTS cc (blockAddressToIV blockN blockOffset) 0 bs

-- | Decrypt using VhdCryptContext
vhdDecrypt :: VhdCryptContext -> VirtualBlockAddress -> BlockByteAddress -> B.ByteString -> B.ByteString
vhdDecrypt (VhdCryptContext cc) blockN blockOffset bs =
    decryptXTS cc (blockAddressToIV blockN blockOffset) 0 bs

-- | Create an IV in big endian mode of the virtual block address
blockAddressToIV :: VirtualBlockAddress -> BlockByteAddress -> B.ByteString
blockAddressToIV (VirtualBlockAddress n) (BlockByteAddress bba) = B.pack [a,b,c,d,0,0,0,0,0,0,0,0,0,0,0,0]
  where a = fromIntegral s
        b = fromIntegral (s `shiftR` 8)
        c = fromIntegral (s `shiftR` 16)
        d = fromIntegral (s `shiftR` 24)
        s = 2*2*1024*n + (bba `div` 512) -- FIXME de-hardcode : blocksize and sector size
