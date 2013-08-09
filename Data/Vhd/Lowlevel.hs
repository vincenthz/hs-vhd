{-# LANGUAGE OverloadedStrings #-}

module Data.Vhd.Lowlevel
    ( readHeader
    , readFooter
    , writeHeader
    , writeFooter
    ) where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Serialize
import Data.Vhd.Checksum
import Data.Vhd.Types
import Prelude hiding (subtract)
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
