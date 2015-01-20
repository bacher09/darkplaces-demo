module DarkPlaces.Demo (
    getDemoMessage,
    getDemoMessages,
    iterDemoMessages,
    demoFileMessages,
    demoFilePackets
) where
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import DarkPlaces.Types
import DarkPlaces.Binary
import DarkPlaces.PacketParser


getDemoMessage :: Get (QVector, BL.ByteString)
getDemoMessage = do
    size <- fromIntegral <$> getWord32le
    angls <- getQVector
    msg <- getLazyByteString size
    return (angls, msg)


getDemoMessages :: Get [(QVector, BL.ByteString)]
getDemoMessages = do
    empty <- isEmpty
    if empty
        then return []
        else (:) <$> getDemoMessage <*> getDemoMessages


iterDemoMessages :: BL.ByteString -> [Either ErrorInfo (QVector, BL.ByteString)]
iterDemoMessages demo_data = go decoder $ BL.toChunks demo_data
  where
    decoder = runGetIncremental getDemoMessage
    go (Fail _ offset msg) _ = [Left (offset, msg)]
    go (Partial k) [] = go (k Nothing) []
    go (Partial k) (x:xs) = go (k $ Just x) xs
    go (Done left _ res) xs = Right res : if empty then [] else go decoder xs'
      where
        empty = B.null left && null xs
        xs' = left:xs


demoFileMessages :: BL.ByteString -> [Either ErrorInfo (QVector, BL.ByteString)]
demoFileMessages file_data = either (\l -> [Left l]) iterDemoMessages either_demo
  where
    either_demo = skipTrack file_data


demoFilePackets :: BL.ByteString -> [Either ErrorInfo PacketOrError]
demoFilePackets file_data = parse (demoFileMessages file_data) defaultDemoState
  where
    parse (x:xs) s = case x of
        Left l -> [Left l]
        Right (_, ps) -> case listPackets ps s of
            Left l -> [Left l]
            Right (pls, s') -> (Right <$> pls) ++ parse xs s'

    parse [] _ = []
