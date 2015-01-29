{-# LANGUAGE BangPatterns #-}
module DarkPlaces.DemoMetadata (
    DemoMetadata(..),
    MetadataList,
    getMetadata,
    getMapname
) where
import Data.Maybe
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Text.Regex.TDFA
import DarkPlaces.PacketParser
import DarkPlaces.Binary (ErrorInfo)
import DarkPlaces.Demo (demoFilePackets)


data DemoMetadata = MapName String
                  | DemoTime Float
                  | DemoMessage Float BL.ByteString
                  | CurlDownload BL.ByteString BL.ByteString BL.ByteString
    deriving(Show, Eq)


type MetadataList = [DemoMetadata]


data DTimeState = DTimeBegin
                | DTimeState Float Float
    deriving(Show, Eq)


timeInit :: DTimeState
timeInit = DTimeBegin

timeUpdate :: DPServerPacket -> DTimeState -> DTimeState
timeUpdate (DPTime x) DTimeBegin = DTimeState x 0
timeUpdate (DPTime x) (DTimeState b _) = DTimeState b x
timeUpdate _ s = s

timeMetadata :: DTimeState -> MetadataList
timeMetadata (DTimeState b e) = [DemoTime (e - b)]
timeMetadata _ = []

timeValue :: DTimeState -> Float
timeValue (DTimeState b e) = e - b
timeValue _ = 0

removePrefix :: BL.ByteString -> BL.ByteString -> Maybe BL.ByteString
removePrefix pref text = if matched then Just removed else Nothing
  where
    pl = BL.length pref
    matched = BL.isPrefixOf pref text
    removed = BL.drop pl text


removeSuffix :: BL.ByteString -> BL.ByteString -> Maybe BL.ByteString
removeSuffix suf text = if matched then Just removed else Nothing
  where
    text_len = BL.length text
    suf_len = BL.length suf
    matched = BL.isSuffixOf suf text
    removed = BL.take (text_len - suf_len) text


removePrefixSuffix :: BL.ByteString -> BL.ByteString -> BL.ByteString -> Maybe BL.ByteString
removePrefixSuffix pref suf text = removePrefix pref text >>= removeSuffix suf


mapName :: BL.ByteString -> Maybe BL.ByteString
mapName model = removePrefixSuffix map_prefix map_suffix model
  where
    map_suffix = BLC.pack ".bsp"
    map_prefix = BLC.pack "maps/"


detectMapName :: DPServerPacket -> MetadataList
detectMapName (DPServerInfo (Right p@(DPServerInfoData {}))) = if not empty
        then map MapName map_names
        else []
  where
    models = dpmodelsPrecached p
    empty = null models
    map_names = maybeToList $ BLC.unpack <$> mapName (head models)

detectMapName _ = []


detectDemoMessage :: DPServerPacket -> Float -> MetadataList
detectDemoMessage (DPPrint p) t = [DemoMessage t p]
detectDemoMessage _ _ = []


detectCurlDownload :: DPServerPacket -> MetadataList
detectCurlDownload (DPStuffText t) = getCurlDownload t
  where
    curlRegex = makeRegex "^curl.*\\-\\-as ([^ ]+) \\-\\-for ([^ ]+) ([^ ]+)" :: Regex
    matchCurl s = match curlRegex s :: (BL.ByteString, BL.ByteString, BL.ByteString, [BL.ByteString])
    getCurlDownload s = case matchCurl s of
        (_, mt, a, [as, for, url]) | not (BL.null mt) -> (CurlDownload as for url) : getCurlDownload a
        _ -> []

detectCurlDownload _ = []


getMetadata :: BL.ByteString -> [Either ErrorInfo DemoMetadata]
getMetadata file_data = go (demoFilePackets file_data) timeInit
  where
    go ((Left x):_) _ = [Left x]
    go ((Right (Right x)):xs) !s = addSimpleMetadata x s ++ go xs (timeUpdate x s)
    go ((Right (Left _)):xs) !s = go xs s
    go [] s = Right <$> timeMetadata s
    addSimpleMetadata x s = Right <$> (detectMapName x ++ detectCurlDownload x ++ (detectDemoMessage x $ timeValue s))


getMapname :: BL.ByteString -> Either ErrorInfo (Maybe String)
getMapname file_data = go (demoFilePackets file_data)
  where
    go ((Left x):_) = Left x
    go ((Right (Right x)):xs) = if not (null mapList) then Right $ maybeMap mapList else go xs
      where
        mapList = detectMapName x
        maybeMap [MapName m] = Just m
        maybeMap _ = Nothing

    go ((Right (Left _)):xs) = go xs
    go [] = Right Nothing
