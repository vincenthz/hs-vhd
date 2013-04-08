module Data.Vhd.Time
    ( VhdDiffTime(..)
    , getVHDTime
    , toPosixSeconds
    , toUTCTime
    ) where

import Data.Word
import Control.Applicative
import Data.Time.Clock.POSIX
import Data.Time.Clock

-- | Represent number of seconds since VHD epoch.
newtype VhdDiffTime = VhdDiffTime Word32
    deriving (Show,Read,Eq,Ord)

y2k :: Word64
y2k = 946684800 -- seconds from the unix epoch to the vhd epoch 

-- | return the current time in vhd epoch time.
getVHDTime :: IO VhdDiffTime
getVHDTime = do
    nowUnixEpoch <- fromIntegral . fromEnum <$> getPOSIXTime
    return $ VhdDiffTime $ fromIntegral (nowUnixEpoch - y2k)

toPosixSeconds :: VhdDiffTime -> POSIXTime
toPosixSeconds (VhdDiffTime ts) = fromIntegral (fromIntegral ts + y2k)

toUTCTime :: VhdDiffTime -> UTCTime
toUTCTime = posixSecondsToUTCTime . toPosixSeconds 
