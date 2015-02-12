module DarkPlaces.Binary  where
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (getLine)
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Int
import DarkPlaces.Types


maxTrackLen :: Int64
maxTrackLen = 8


getQVector :: Get QVector
getQVector = consQVector <$> getFloat32le <*> getFloat32le <*> getFloat32le


getLine :: Get BL.ByteString
getLine = do
    b <- getWord8
    if b == 10  -- 10 is '\n'
        then return $ BL.singleton b
        else BL.cons' b <$> getLine


getLineLimited :: Int64 -> Get BL.ByteString
getLineLimited limit | limit <= 0 = fail "Line to long"
                     | otherwise = do
    b <- getWord8
    if b == 10
        then return $ BL.singleton b
        else BL.cons' b <$> getLineLimited (limit - 1)


getStringList :: Get [BL.ByteString]
getStringList = do
    str <- getLazyByteStringNul
    if BL.null str
        then return []
        else (str :) <$> getStringList


-- signed char
getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

-- signed short
getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

-- signed int
getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

getAngle8i :: Get Float
getAngle8i = (360.0 / 256.0 *) . fromIntegral <$> getInt8

getAngle16i :: Get Float
getAngle16i = (360.0 / 65536.0 *) . fromIntegral <$> getInt16le

getCord16i :: Get Float
getCord16i = fromIntegral <$> getInt16le

-- unsigned char to int
getWord8asInt :: Get Int
getWord8asInt = fromIntegral <$> getWord8

-- signed char to int
getInt8asInt :: Get Int
getInt8asInt = fromIntegral <$> getInt8

getInt16asInt :: Get Int
getInt16asInt = fromIntegral <$> getInt16le

getWord16asInt :: Get Int
getWord16asInt = fromIntegral <$> getWord16le

getLineAndRemaining :: Get (BL.ByteString, Int64)
getLineAndRemaining = getLine >>= \l -> bytesRead >>= \b -> return (l, b)

getLineLAndRemaining :: Int64 -> Get (BL.ByteString, Int64)
getLineLAndRemaining n = getLineLimited n >>= \l -> bytesRead >>= \b -> return (l, b)

splitAtTrack :: BL.ByteString -> Either ErrorInfo (BL.ByteString, BL.ByteString)
splitAtTrack file_data = case either_track of
    Left (_, offset, msg) -> Left (offset, msg)
    Right (_, _, (line, drop_bytes)) -> Right (line, BL.drop drop_bytes file_data)
  where
    either_track = runGetOrFail (getLineLAndRemaining maxTrackLen) file_data

skipTrack :: BL.ByteString -> Either ErrorInfo BL.ByteString
skipTrack file_data = snd <$> splitAtTrack file_data
