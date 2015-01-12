module DarkPlaces.Binary  where
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Prelude hiding (getLine)
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Int
import DarkPlaces.Types


getQVector :: Get QVector
getQVector = consQVector <$> getFloat32le <*> getFloat32le <*> getFloat32le


getLine :: Get L.ByteString
getLine = do
    b <- getWord8
    if b == 10  -- 10 is '\n'
        then return $ L.singleton b
        else L.cons' b <$> getLine


getStringList :: Get [L.ByteString]
getStringList = do
    str <- getLazyByteStringNul
    if L.null str
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
