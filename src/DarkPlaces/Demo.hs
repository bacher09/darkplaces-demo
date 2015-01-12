module DarkPlaces.Demo (
    getDemoMessage,
    getDemoMessages
) where
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import DarkPlaces.Types
import DarkPlaces.Binary


getDemoMessage :: Get (QVector, L.ByteString)
getDemoMessage = do
    size <- fromIntegral <$> getWord32le
    angls <- getQVector
    msg <- getLazyByteString size
    return (angls, msg)


getDemoMessages :: Get [(QVector, L.ByteString)]
getDemoMessages = do
    empty <- isEmpty
    if empty
        then return []
        else (:) <$> getDemoMessage <*> getDemoMessages
