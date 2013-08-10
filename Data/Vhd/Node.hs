{-# LANGUAGE OverloadedStrings #-}

module Data.Vhd.Node
    ( VhdNode (..)
    , getVhdBlockMapper
    , containsBlock
    , lookupOrCreateBlock
    , openCryptKey
    , withVhdNode
    , withMappedBlock
    , iterateBlocks
    , iterateBlockSectors
    , batmapHeaderChange
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.IORef
import Data.Serialize (decode, encode)
import Data.Vhd.Block
import Data.Vhd.Header
import Data.Vhd.Batmap
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

-- | return the (reading, writing) blockDataMapper for a giving node
getVhdBlockMapper :: VhdNode -> (Maybe BlockDataMapper, Maybe BlockDataMapper)
getVhdBlockMapper node =
    (vhdDecrypt `fmap` nodeCryptCtx node, vhdEncrypt `fmap` nodeCryptCtx node)

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
        -- make sure the key match
        case key of
            Nothing -> return ()
            Just k  -> case batmapHeaderKeyHash `fmap` mBatmapHdr of
                        Just (KeyHash (Just (nonce, expected))) -> do
                            when (expected /= calculateHash nonce k) $ error "keyhash differ"
                        _             -> return ()
        -- now mmap the bat
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
            return a
  where initCryptContext ck = maybe (error "invalid crypt key") id $ vhdCryptInit ck

-- | Return the physical sector of a specific block, or create a new one if this block
-- has not been allocated yet
lookupOrCreateBlock :: VhdNode -> VirtualBlockAddress -> IO PhysicalSectorAddress
lookupOrCreateBlock node blockNumber = do
    mpsa <- Bat.lookupBlock (nodeBat node) blockNumber
    case mpsa of
        Nothing  -> appendEmptyBlock node blockNumber
        Just psa -> return psa

containsBlock :: VhdNode -> VirtualBlockAddress -> IO Bool
containsBlock node = Bat.containsBlock (nodeBat node)

batmapHeaderChange :: VhdNode -> (BatmapHeader -> BatmapHeader) -> IO ()
batmapHeaderChange node f = Bat.batmapHeaderModify (nodeBat node) f

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

-- | Iterate Present blocks in a vhd file
iterateBlocks :: VhdNode          -- ^ the vhd file
              -> (Block -> IO ()) -- ^ callback
              -> IO ()
iterateBlocks vhd f = mapM_ callAt [0..(nbBlocks-1)]
  where nbBlocks = VirtualBlockAddress $ headerMaxTableEntries $ nodeHeader vhd
        callAt vba = do
            mpsa <- Bat.lookupBlock (nodeBat vhd) vba
            case mpsa of
                Nothing  -> return ()
                Just psa -> withMappedBlock vhd psa vba f

-- | Iterate sectors in a specific block
iterateBlockSectors :: VhdNode             -- ^ the vhd file
                    -> VirtualBlockAddress -- ^ block address
                    -> (Block -> BlockSectorAddress -> Bool -> IO ()) -- ^ callback
                    -> IO ()
iterateBlockSectors vhd vba f = do
    mpsa <- Bat.lookupBlock (nodeBat vhd) vba
    case mpsa of
        Nothing  -> return ()
        Just psa -> withMappedBlock vhd psa vba (\block -> iterateSectors block (f block))
