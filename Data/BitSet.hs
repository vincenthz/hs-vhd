module Data.BitSet
    ( BitSet
    , empty
    , fromByteString
    , fromRange
    , toList
    , isEmpty
    , intersect
    , union
    , subtract
    )
    where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Prelude hiding (subtract)

data BitSet a = BitSet B.ByteString

instance Eq (BitSet a) where
    (BitSet b1) == (BitSet b2) = b1' == b2'
        where (b1', b2') = byteStringsPad b1 b2

instance Show a => Show (BitSet a) where
    show = show . toList

empty :: Num a => BitSet a
empty = BitSet B.empty

fromByteString :: Num a => B.ByteString -> BitSet a
fromByteString = BitSet

fromRange :: Int -> Int -> BitSet a
fromRange lo hi = BitSet generate where

    generate
        | lo <  0  = error "lower bound cannot be less than zero."
        | lo >  hi = error "lower bound cannot be greater than upper bound."
        | lo == hi = B.empty
        | lo    == 0 && hiBit == 0 = setBytes
        | loBit == 0 && hiBit == 0 = B.concat [clearBytes, setBytes]
        | loBit == 0 && hiBit /= 0 = B.concat [clearBytes, setBytes, fallByte]
        | loBit /= 0 && hiBit == 0 = B.concat [clearBytes, riseByte, setBytes]
        | loByteFloor   == hiByteFloor = B.concat [clearBytes, humpByte]
        | loByteCeiling == hiByteFloor = B.concat [clearBytes, riseByte, fallByte]
        | loByteCeiling <  hiByteFloor = B.concat [clearBytes, riseByte, setBytes, fallByte]

    (loByteFloor, loBit) = lo `divMod` 8
    (hiByteFloor, hiBit) = hi `divMod` 8
    loByteCeiling = (lo + 7) `div` 8
    hiByteCeiling = (hi + 7) `div` 8

    clearBytes = B.replicate (fromIntegral loByteFloor) 0x00
    setBytes   = B.replicate (fromIntegral (hiByteFloor - loByteCeiling)) 0xff

    riseByte = B.singleton $ setBits 0 loBit     8
    fallByte = B.singleton $ setBits 0     0 hiBit
    humpByte = B.singleton $ setBits 0 loBit hiBit

toList :: BitSet a -> [Int]
toList (BitSet b) = map snd $ filter fst $ zip (byteStringBits b) [0..]

isEmpty :: BitSet a -> Bool
isEmpty (BitSet b) = B.all (== 0) b

intersect :: BitSet a -> BitSet a -> BitSet a
union     :: BitSet a -> BitSet a -> BitSet a
subtract  :: BitSet a -> BitSet a -> BitSet a

intersect = binaryOp (.&.)
union     = binaryOp (.|.)
subtract  = binaryOp (\x y -> x .&. complement y)

binaryOp f (BitSet b1) (BitSet b2) =
    BitSet $ byteStringPackZipWith f b1' b2'
  where (b1', b2') = byteStringsPad b1 b2

byteStringBits byteString = do
    word <- B.unpack byteString
    bit <- word8Bits word
    return bit

byteStringPackZipWith :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
byteStringPackZipWith = ((B.pack .) .) . B.zipWith

byteStringsPad :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
byteStringsPad b1 b2 =
    if length1 < length2
        then (B.append b1 (B.replicate (length2 - length1) 0), b2)
        else (b1, B.append b2 (B.replicate (length1 - length2) 0))
    where
        length1 = B.length b1
        length2 = B.length b2

byteStringsContract :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
byteStringsContract b1 b2 =
    if length1 < length2
        then (b1, B.take length1 b2)
        else (B.take length2 b1, b2)
    where
        length1 = B.length b1
        length2 = B.length b2

setBits :: Bits a => a -> Int -> Int -> a
setBits acc loBit hiBit
    | loBit < hiBit = setBits (setBit acc loBit) (loBit + 1) hiBit
    | otherwise     = acc

word8Bits :: Word8 -> [Bool]
word8Bits w = map (testBit w) [0 .. 7]

