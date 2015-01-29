module Main where
import Options.Applicative
import DarkPlaces.DemoMetadata
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import System.Exit
import Control.Monad.Error
import Text.Printf

data CommandArgs = CommandArgs {
    onlyMapname :: Bool,
    filename :: String
} deriving(Show, Eq)

type MaybeError = ErrorT String IO


argsParser :: Parser CommandArgs
argsParser = CommandArgs 
    <$> switch
        ( long "map"
        <> short 'm'
        <> help "Print only map name")
    <*> argument str (
        metavar "FILE"
        <> help "Input demo file")

openFile :: CommandArgs -> MaybeError BL.ByteString
openFile args = do
    let file = filename args
    exist <- liftIO $ doesFileExist file
    if exist
        then liftIO $ BL.readFile file
        else throwError $ printf "File \"%s\" does not exists\n" file


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
        else return ()


demoInfo :: CommandArgs -> IO ()
demoInfo args = do
    r <- runErrorT $ processDemo args
    case r of
        Right _ -> print args
        Left e -> putStrLn e >> exitFailure


main :: IO ()
main = demoInfo =<< execParser opts
  where
    opts = info (helper <*> argsParser)
        (fullDesc
        <> progDesc "Get demo file metada")
