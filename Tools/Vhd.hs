import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.IORef
import Data.Vhd
import Data.Vhd.Lowlevel
import Data.Vhd.Bat
import qualified Data.Vhd.Block as Block
import Data.Vhd.Checksum
import Data.Vhd.Node
import Data.Vhd.Types
import System.Environment (getArgs)
import System.IO
import Text.Printf

cmdConvert [fileRaw, fileVhd, size] = convert =<< rawSizeBytes where
    vhdSizeMiB   = read size
    vhdSizeBytes = vhdSizeMiB * 1024 * 1024
    rawSizeBytes = fmap fromIntegral $ withFile fileRaw ReadMode hFileSize
    convert rawSizeBytes
        | vhdSizeMiB `mod` 2 /= 0 = error
            "specified VHD size is not a multiple of 2 MiB."
        | vhdSizeBytes < rawSizeBytes = error
            "specified VHD size is not large enough to contain raw data."
        | otherwise = do
            create fileVhd $ defaultCreateParameters
                { createVirtualSize = vhdSizeBytes }
            withVhd fileVhd $ \vhd -> BL.readFile fileRaw >>= writeDataRange vhd 0
cmdConvert _ = error "usage: convert <raw file> <vhd file> <size MiB>"

cmdCreate [name, size] =
    create name $ defaultCreateParameters
        { createVirtualSize = read size * 1024 * 1024 }
cmdCreate _ = error "usage: create <name> <size MiB>"

cmdExtract [fileVhd, fileRaw] = withVhd fileVhd $ readData >=> BL.writeFile fileRaw
cmdExtract _                  = error "usage: extract <vhd file> <raw file>"

cmdPropGet [file, key] = withVhdNode file $ \node -> do
    case map toLower key of
        "max-table-entries"   -> putStrLn $ show $ headerMaxTableEntries   $ nodeHeader node
        "blocksize"           -> putStrLn $ show $ headerBlockSize         $ nodeHeader node
        "disk-type"           -> putStrLn $ show $ footerDiskType          $ nodeFooter node
        "current-size"        -> putStrLn $ show $ footerCurrentSize       $ nodeFooter node
        "uuid"                -> putStrLn $ show $ footerUniqueId          $ nodeFooter node
        "parent-uuid"         -> putStrLn $ show $ headerParentUniqueId    $ nodeHeader node
        "parent-timestamp"    -> putStrLn $ show $ headerParentTimeStamp   $ nodeHeader node
        "parent-filepath"     -> putStrLn $ show $ headerParentUnicodeName $ nodeHeader node
        "timestamp"           -> putStrLn $ show $ footerTimeStamp         $ nodeFooter node
        _                     -> error "unknown key"
cmdPropGet _ = error "usage: prop-get <file> <key>"

cmdRead [file] = withVhdNode file $ \node -> do
    let hdr = nodeHeader node
    let ftr = nodeFooter node
    mapM_ (\(f, s) -> putStrLn (f ++ " : " ++ s))
        [ ("cookie           ", show $ headerCookie hdr)
        , ("version          ", show $ headerVersion hdr)
        , ("max-table-entries", show $ headerMaxTableEntries hdr)
        , ("block-size       ", showBlockSize $ headerBlockSize hdr)
        , ("header-checksum  ", showChecksum (headerChecksum hdr) (verifyHeaderChecksum hdr))
        , ("parent-uuid      ", show $ headerParentUniqueId hdr)
        , ("parent-filepath  ", show $ headerParentUnicodeName hdr)
        , ("parent-timestamp ", show $ headerParentTimeStamp hdr)
        ]
    let (ParentLocatorEntries ents) = headerParentLocatorEntries hdr
    putStrLn "locators:"
    forM_ (zip ents [0..]) $ \(ent,i) -> do
        printf "  %d: code=%.8x space=%.8x len=%.8x offset=%.16x\n"
                (i :: Int) (locatorCode ent) (locatorDataSpace ent) (locatorDataSpace ent) (locatorDataOffset ent)
        return ()

    mapM_ (\(f, s) -> putStrLn (f ++ " : " ++ s))
        [ ("disk-geometry    ", show $ footerDiskGeometry ftr)
        , ("original-size    ", showBlockSize $ footerOriginalSize ftr)
        , ("current-size     ", showBlockSize $ footerOriginalSize ftr)
        , ("type             ", show $ footerDiskType ftr)
        , ("footer-checksum  ", showChecksum (footerChecksum ftr) (verifyFooterChecksum ftr))
        , ("uuid             ", show $ footerUniqueId ftr)
        , ("timestamp        ", show $ footerTimeStamp ftr)
        ]
    allocated <- newIORef 0
    batIterate (nodeBat node) (fromIntegral $ headerMaxTableEntries hdr) $ \i n -> do
        unless (n == 0xffffffff) $ modifyIORef allocated ((+) 1) >> printf "BAT[%.5x] = %08x\n" i n
    nb <- readIORef allocated
    putStrLn ("blocks allocated  : " ++ show nb ++ "/" ++ show (headerMaxTableEntries hdr))
cmdRead _ = error "usage: read <file>"

cmdSnapshot [fileVhdParent, fileVhdChild] =
    withVhd fileVhdParent $ \vhdParent -> snapshot vhdParent fileVhdChild
cmdSnapshot _ = error "usage: snapshot <parent vhd file> <child vhd file>"

showBlockSize i
    | i < 1024     = printf "%d bytes" i
    | i < (1024^2) = printf "%d KiB" (i `div` 1024)
    | i < (1024^3) = printf "%d MiB" (i `div` (1024^2))
    | otherwise    = printf "%d GiB" (i `div` (1024^3))

showChecksum checksum isValid =
    printf "%08x (%s)" checksum (if isValid then "valid" else "invalid")

cmdHelp _ = usage Nothing

cmdPropSetUuid args = do
    let (file,uuidStr) = case args of
                        [f, u] -> (f,u)
                        _      -> error "usage: vhd <set-uuid> <file> <newuuid>"
    let uuid = read uuidStr
    footer <- either error id <$> readFooter file
    writeFooter file $ footer { footerUniqueId = uuid }

knownCommands =
    [ ("convert", cmdConvert)
    , ("create",  cmdCreate)
    , ("extract", cmdExtract)
    , ("prop-get",cmdPropGet)
    , ("set-uuid",cmdPropSetUuid)
    , ("read"    ,cmdRead)
    , ("snapshot",cmdSnapshot)
    , ("help"    ,cmdHelp)
    ]

usage msg = do
    maybe (return ()) putStrLn msg
    putStrLn "usage: vhd <command>"
    putStrLn ""
    mapM_ (putStrLn . ("  " ++) . fst) knownCommands

main = do
    args <- getArgs
    case args of
        []       -> usage Nothing
        cmd : xs -> case lookup cmd knownCommands of
                        Nothing -> usage $ Just ("unknown command: " ++ cmd)
                        Just f  -> f xs
