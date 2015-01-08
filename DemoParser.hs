module DemoParser (
    getDemoMessage,
    getDemoMessages,
    getLine
) where
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Binary.Get
import Prelude hiding (getLine)
import DPTypes
import PacketParser


getLine :: Get L.ByteString
getLine = do
    b <- getWord8
    if b == 10  -- 10 is '\n'
        then return $ L.singleton b
        else L.cons' b <$> getLine


getDemoMessage = do
    size <- fromIntegral <$> getWord32le
    angls <- getQVector
    msg <- getLazyByteString size
    return (angls, msg)


getDemoMessages = do
    empty <- isEmpty
    if empty
        then return []
        else (:) <$> getDemoMessage <*> getDemoMessages


{-*Main System.IO.Unsafe> let mdata = unsafePerformIO $ L.readFile "../slava_qbit_gib-warehouse.dem"-}
{-*Main System.IO.Unsafe> let pm = runGet (getLine >> getDemoMessages) mdata-}
{-*Main System.IO.Unsafe> let messages = Prelude.map snd pm-}
{-*Main System.IO.Unsafe> let packets = Prelude.map (runGet parsePackets) messages -}
{-*Main System.IO.Unsafe> mapM_ print (take 100 packets)-}
