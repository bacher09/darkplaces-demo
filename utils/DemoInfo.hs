module Main where
import Options.Applicative
import DarkPlaces.DemoMetadata
import DarkPlaces.Text
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Directory (doesFileExist)
import System.Exit
import Control.Monad.Error
import Text.Printf
import Data.Either
import Data.Fixed (mod')
import Control.Exception (catch)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Concurrent.Async
import Data.String


data CommandArgs = CommandArgs {
    onlyMapname :: Bool,
    checkUrls :: Bool,
    colorsOutput :: Bool,
    filename :: String
} deriving(Show, Eq)


data ResourceDownload = ResourceDownload {
    as :: String,
    for :: String,
    url :: String
} deriving (Show, Eq)

data FileMetadata = FileMetadata {
    mapName :: String,
    demoTime :: Float,
    downloads :: [ResourceDownload]
} deriving (Show, Eq)

type MaybeError = ErrorT String IO

argsParser :: Parser CommandArgs
argsParser = CommandArgs 
    <$> switch
        ( long "map"
        <> short 'm'
        <> help "Print only map name")
    <*> switch
        ( long "urls"
        <> short 'u'
        <> help "Check download urls")
    <*> switch
        ( long "colors"
        <> short 'c'
        <> help "Output colorful text ever if stdout is not terminal device")
    <*> argument str (
        metavar "FILE"
        <> help "Input demo file")


printMessages :: Bool -> MetadataList -> IO ()
printMessages col metadata =  putStrLn "Messages:" >> (mapM_ print $ filter pred metadata)
  where
    pred (DemoMessage _ _) = True
    pred _ = False
    print (DemoMessage _ m) = printDPText col m
    print _ = return ()


openFile :: CommandArgs -> MaybeError BL.ByteString
openFile args = do
    let file = filename args
    exist <- liftIO $ doesFileExist file
    if exist
        then liftIO $ BL.readFile file
        else throwError $ printf "File \"%s\" does not exists\n" file


formatTime :: Float -> String
formatTime d
    | d <= 60 = s_repr
    | d <= (60 * 60) =  printf "%s %s" m_repr s_repr
    | otherwise = printf "%s %s %s" h_repr m_repr s_repr
  where
    s = d `mod'` 60 :: Float
    m = (truncate $ d / 60) `rem` 60 :: Int
    h = (truncate $ d / (60 * 60)) `rem` 24 :: Int
    s_repr = choose "second" "seconds" s
    m_repr = choose "minute" "minutes" m
    h_repr = choose "hour" "hours" h
    choose s1 s2 v
        | v == 1 = show v ++ " " ++ s1
        | otherwise = show v ++ " " ++ s2


checkUrl :: String -> Manager -> IO Bool
checkUrl url m = catch urlResponse (\(StatusCodeException _ _ _) -> return False)
  where
    redir_count = 10
    headRequest url = do
        r <- parseUrl url
        return $ r {method=fromString "HEAD", redirectCount=redir_count}

    urlResponse = do
        req <- headRequest url
        _ <- httpNoBody req m -- get response
        return True


formatMetadata :: MetadataList -> CommandArgs -> MaybeError ()
formatMetadata metadata args = do
    liftIO $ putStrLn $ printf "Map:      %s" $ mapName meta
    liftIO $ putStrLn $ printf "Time:     %s" $ formatTime $ demoTime meta
    liftIO $ printUrls meta
    liftIO $ printMessages colorful metadata
    return ()
  where
    meta = foldMetadata metadata initState
    check_urls = checkUrls args
    colorful = colorsOutput args
    printUrls = if check_urls then printDownloadsWithCheck colorful else printDownloads
    initState = FileMetadata "" 0 []
    foldMetadata ((MapName m):xs) ms = foldMetadata xs $ ms {mapName=m}
    foldMetadata ((DemoTime t):xs) ms = foldMetadata xs $ ms {demoTime=t}
    foldMetadata ((CurlDownload as for url):xs) ms = foldMetadata xs ms'
      where
        ms' = ms {downloads=(ResourceDownload (BLC.unpack as) (BLC.unpack for) (BLC.unpack url)): downloads ms}
    foldMetadata (_:xs) ms = foldMetadata xs ms
    foldMetadata [] ms = ms {downloads= reverse $ downloads ms}
    printDownloads meta = mapM_ printDownload $ downloads meta
      where
        printDownload d = putStrLn $ printf "Download: %s -- as %s" (url d) (as d)
    printDownloadsWithCheck col meta = do
        m <- newManager tlsManagerSettings
        down <- forM (downloads meta) $ \d -> do
            aok <- async $ checkUrl (url d) m
            return (d, aok)

        forM_ down $ \(d, aok) -> do
            ok <- wait aok
            let ok_str = fromString $ if ok then "^2OK" else "^1BROKEN"
            putStr $ printf "Download: %s -- as %s is " (url d) (as d)
            printDPText col ok_str >> putStrLn ""


processDemo :: CommandArgs -> MaybeError ()
processDemo args = do
    file_data <- openFile args
    if onlyMapname $ args
        then do
            let maybe_map = getMapname file_data
            case maybe_map of
                (Right (Just m)) -> liftIO $ putStrLn $ printf "Map: %s" m
                (Right Nothing) -> liftIO $ putStrLn "No map information in demo file"
                (Left _) -> throwError "Error during parsing file"
        else do
            let (errors, metadata) = partitionEithers $ getMetadata file_data
            if null errors
                then formatMetadata metadata args
                else throwError "Error during parsing file"


demoInfo :: CommandArgs -> IO ()
demoInfo args = do
    r <- runErrorT $ processDemo args
    case r of
        Right _ -> exitSuccess
        Left e -> putStrLn e >> exitFailure


main :: IO ()
main = demoInfo =<< execParser opts
  where
    opts = info (helper <*> argsParser)
        (fullDesc
        <> progDesc "Get demo file metada")
