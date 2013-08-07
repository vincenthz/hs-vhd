module Data.Vhd.UniqueId
    ( UniqueId
    , uniqueId
    , randomUniqueId
    ) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Byteable
import System.Random
import qualified Data.ByteString as B
import Text.Printf

newtype UniqueId = UniqueId B.ByteString deriving (Eq)

-- | smart constructor for uniqueId
uniqueId i = assert (B.length i ==  16) $ UniqueId             i

instance Byteable UniqueId where
    toBytes (UniqueId b) = b

instance Show UniqueId where
    show (UniqueId b) = intercalate "-" $ map disp [[0 .. 3], [4, 5], [6, 7], [8, 9], [10 .. 15]]
        where disp = concatMap (printf "%02x" . B.index b)

instance Read UniqueId where
    readsPrec _ r = let (t,d) = splitAt 36 r
                     in case ofString t of
                            Nothing  -> []
                            Just uid -> [(uid, d)]

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

randomUniqueId :: IO UniqueId
randomUniqueId
    = liftM (uniqueId . B.pack)
    $ replicateM 16
    $ liftM fromIntegral
    $ randomRIO (0 :: Int, 255)
