module Main where
import Criterion.Main
import Prelude hiding (getLine)
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.State.Lazy
import DarkPlaces.PacketParser
import DarkPlaces.Demo
import Data.Word
import Data.Binary.Get
import Data.Either


testDemo :: String
testDemo = "test_data/eac_cup_ex2perts_vs_schlitzerbos_asgy_gib_warehouse.dem"


parseDemo1 :: FilePath -> IO [[Either Word8 DPServerPacket]]
parseDemo1 file_name = do
    file_data <- BL.readFile file_name
    return $ parseFile file_data
  where
    msgs input = map snd $ runGet getDemoMessages (BL.drop 3 input)
    parseMsg x s = runGet (runStateT parsePackets s) x
    parseData (x:xs) s = let (xr, s') = parseMsg x s
                             in xr : parseData xs s'
    parseData [] _ = []
    parseFile input = parseData (msgs input) defaultDemoState


parseDemo2 :: FilePath -> IO [[Either Word8 DPServerPacket]]
parseDemo2 file_name = do
    file_data <- BL.readFile file_name
    return $ parseFile $ BL.drop 3 file_data
  where
    parseFile file_chunks = go (messages file_chunks) defaultDemoState
    messages demo_data = map snd $ rights $ iterDemoMessages demo_data
    goMessages msg s = runGet (runStateT parsePackets s) msg
    go (x:xs) s = let (res, s') = goMessages x s
                  in res: go xs s'

    go [] _ = []



packet_stream :: [[Either Word8 DPServerPacket]] -> [DPServerPacket]
packet_stream msgs = concat $ rights <$> msgs

main :: IO ()
main = defaultMain [
    bgroup "full" [
        bench "length" $ nfIO $ length . packet_stream <$> parseDemo1 testDemo,
        bench "head" $ whnfIO $ head . packet_stream <$> parseDemo1 testDemo,
        bench "last" $ whnfIO $ last . packet_stream <$> parseDemo1 testDemo],
    bgroup "iter" [
        bench "length" $ nfIO $ length . packet_stream <$> parseDemo2 testDemo,
        bench "head" $ whnfIO $ head . packet_stream <$> parseDemo2 testDemo,
        bench "last" $ whnfIO $ last . packet_stream <$> parseDemo2 testDemo]]
