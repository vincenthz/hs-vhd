module Data.Vhd.Checksum
    ( Checksum(..)
    , CheckSumable(..)
    , checksumCalculate
    , verifyChecksum
    , adjustChecksum
    ) where

import Data.Bits
import Data.Monoid
import qualified Data.ByteString as B
import Data.Serialize
import Data.Word

newtype Checksum = Checksum Word32
    deriving (Show,Eq)

instance Serialize Checksum where
    put (Checksum v) = putWord32be v
    get = Checksum `fmap` getWord32be

instance Monoid Checksum where
    mempty = Checksum 0
    mappend (Checksum a) (Checksum b) = Checksum (a+b)

class Serialize a => CheckSumable a where
    calculateChecksum :: a -> Checksum
    getChecksum       :: a -> Checksum
    setChecksum       :: Checksum -> a -> a

adjustChecksum :: CheckSumable a => a -> a
adjustChecksum a = setChecksum checksum a
  where checksum = calculateChecksum a

verifyChecksum :: CheckSumable a => a -> Bool
verifyChecksum a = expected == got
  where expected = getChecksum a
        got      = calculateChecksum a

checksumPlus :: Checksum -> Word8 -> Checksum
checksumPlus (Checksum a) b = Checksum (a + fromIntegral b)

checksumComplement :: Checksum -> Checksum
checksumComplement (Checksum s) = Checksum (complement s)

checksumCalculate :: B.ByteString -> Checksum
checksumCalculate = checksumComplement . B.foldl' checksumPlus (Checksum 0)
