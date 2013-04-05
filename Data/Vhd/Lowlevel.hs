{-# LANGUAGE OverloadedStrings #-}

module Data.Vhd.Lowlevel
    ( readHeader
    , readFooter
    , writeHeader
    , writeFooter
    ) where

import Control.Applicative
import Control.Monad
import Data.BitSet
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Serialize
import Data.Time.Clock.POSIX
import Data.Vhd.Bat
import Data.Vhd.Block hiding (readData, readDataRange, writeDataRange)
import qualified Data.Vhd.Block as Block
import Data.Vhd.Checksum
import Data.Vhd.Geometry
import Data.Vhd.Node
import Data.Vhd.Types
import Data.Vhd.Utils
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Prelude hiding (subtract)
import System.FilePath.Posix
import System.IO

-- | read footer directly from a vhd file.
readFooter :: FilePath -> IO (Either String Footer)
readFooter filepath = withFile filepath ReadMode $ \handle ->
    decode <$> B.hGet handle 512

-- | read header directly from a vhd file
readHeader :: FilePath -> IO (Either String Header)
readHeader filepath = withFile filepath ReadMode $ \handle -> do
    hSeek handle AbsoluteSeek 512
    decode <$> B.hGet handle 1024

-- | re-write both footer in a VHD file
writeFooter :: FilePath -> Footer -> IO ()
writeFooter filePath footer = do
    withFile filePath ReadWriteMode $ \handle -> do
        a <- hTell handle
        when (a /= 0) $  error "aaa"
        B.hPut handle footerBs
        hSeek handle SeekFromEnd 512
        B.hPut handle footerBs
  where
    footerBs = encode $ adjustFooterChecksum footer

-- | re-write an header in a VHD file
writeHeader :: FilePath -> Header -> IO ()
writeHeader filePath header = do
    withFile filePath ReadWriteMode $ \handle -> do
        hSeek handle AbsoluteSeek 512
        B.hPut handle headerBs
  where
    headerBs = encode $ adjustHeaderChecksum header
