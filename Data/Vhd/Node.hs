{-# LANGUAGE OverloadedStrings #-}

module Data.Vhd.Node
    ( VhdNode (..)
    , containsBlock
    , lookupOrCreateBlock
    , withVhdNode
    , withMappedBlock
    , iterateBlockSectors
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.IORef
import Data.Serialize (decode, encode)
import Data.Vhd.Block
import Data.Vhd.Header
import Data.Vhd.Bitmap (bitmapGet)
import qualified Data.Vhd.Bat as Bat
import Data.Vhd.Types
import Data.Vhd.Serialize ()
import Data.Vhd.Utils
import Data.Vhd.Crypt
import System.Directory
import System.IO

-- | Represent one VHD file, possibly part of a VHD chain
data VhdNode = VhdNode
    { nodeBat      :: Bat.Bat
    , nodeHeader   :: Header
    , nodeFooter   :: Footer
    , nodeHandle   :: Handle
    , nodeCryptCtx :: Maybe VhdCryptContext
    , nodeFilePath :: FilePath
    , nodeModified :: IORef Bool
    }

withVhdNode :: FilePath -> (VhdNode -> IO a) -> IO a
withVhdNode filePath f = do
    e   <- doesFileExist filePath
    unless e $ error "file doesn't exist"
    key <- findImplicitCryptKey filePath
    withFile filePath ReadWriteMode $ \handle -> do
        footer <- either error id . decode <$> B.hGet handle 512
        header <- either error id . decode <$> B.hGet handle 1024
        mBatmapHdr <- if footerCreatorVersion footer == Version 1 3
            then do
                -- skip the BAT, and try reading the batmap header
                hSeek handle RelativeSeek (fromIntegral $ Bat.batGetSize header footer)
                batmapHdr <- decode <$> B.hGet handle 512
                case batmapHdr of
                    Left _     -> return Nothing
                    Right bHdr ->
                        if batmapHeaderCookie bHdr == cookie "tdbatmap"
                            then return $ Just bHdr
                            else return Nothing
            else return Nothing
        Bat.batMmap filePath header footer mBatmapHdr $ \bat -> do
            bmodified <- newIORef False
            a <- f $ VhdNode
                { nodeBat      = bat
                , nodeHeader   = header
                , nodeFooter   = footer
                , nodeHandle   = handle
                , nodeCryptCtx = fmap initCryptContext key
                , nodeFilePath = filePath
                , nodeModified = bmodified
                }
            modified <- readIORef bmodified
            when (modified) $ Bat.batUpdateChecksum bat
            return a
  where initCryptContext ck = maybe (error "invalid crypt key") id $ vhdCryptInit ck

lookupOrCreateBlock :: VhdNode -> VirtualBlockAddress -> IO PhysicalSectorAddress
lookupOrCreateBlock node blockNumber = do
    mpsa <- Bat.lookupBlock (nodeBat node) blockNumber
    case mpsa of
        Nothing  -> appendEmptyBlock node blockNumber
        Just psa -> return psa

containsBlock :: VhdNode -> VirtualBlockAddress -> IO Bool
containsBlock node = Bat.containsBlock (nodeBat node)

-- | Create a new empty block at the end of the vhd file
appendEmptyBlock :: VhdNode -> VirtualBlockAddress -> IO PhysicalSectorAddress
appendEmptyBlock node n = do
    -- seek to the end of the file minus the footer
    hSeek (nodeHandle node) SeekFromEnd 512

    pos <- hTell (nodeHandle node)
    let sector = addrToSector (fromIntegral pos)

    Bat.batWrite (nodeBat node) n sector
    modifyIORef (nodeModified node) (const True)
    -- write bitmap, then write block with the optional block data mapper
    B.hPut (nodeHandle node) $ B.replicate bitmapSize 0
    B.hPut (nodeHandle node) $ maybe id (\cc -> vhdEncrypt cc n 0) (nodeCryptCtx node) $ B.replicate (fromIntegral bsz) 0
    -- align to the next sector length and then re-write the footer
    hAlign (nodeHandle node) sectorLength
    B.hPut (nodeHandle node) $ encode (nodeFooter node)
    return sector
  where
        bitmapSize = bitmapSizeOfBlockSize blockSize
        blockSize@(BlockSize bsz)  = headerBlockSize $ nodeHeader node

withMappedBlock :: VhdNode -> PhysicalSectorAddress -> VirtualBlockAddress -> (Block -> IO a) -> IO a
withMappedBlock vhd psa vba f = withBlock (nodeFilePath vhd) blockSize vba psa f
  where blockSize = headerBlockSize $ nodeHeader vhd

iterateBlockSectors :: VhdNode
                    -> VirtualBlockAddress
                    -> (Block -> BlockSectorAddress -> Bool -> IO ())
                    -> IO ()
iterateBlockSectors vhd vba f = do
    mpsa <- Bat.lookupBlock (nodeBat vhd) vba
    case mpsa of
        Nothing  -> return ()
        Just psa -> withMappedBlock vhd psa vba iterateBlockBitmap
    where iterateBlockBitmap block =
              forM_ [0..(nbSectors-1)] $ \sector@(BlockSectorAddress bsa) ->
                  bitmapGet (bitmapOfBlock block) (fromIntegral bsa) >>= f block sector
            where nbSectors = sectorPerBlock block
