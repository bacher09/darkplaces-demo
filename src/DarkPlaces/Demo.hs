module DarkPlaces.Demo (
    getDemoMessage,
    getDemoMessages,
    iterDemoMessages
) where
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import DarkPlaces.Types
import DarkPlaces.Binary


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


iterDemoMessages :: BL.ByteString -> [Either (ByteOffset, String) (QVector, BL.ByteString)]
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
