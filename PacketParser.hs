module PacketParser (
    DPServerPacket(..),
    parsePacket,
    parsePackets
) where

import Prelude hiding (sequence)
import Control.Monad hiding (sequence, mapM)
import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as SM
import Data.Traversable



data ProtocolVersion = ProtocolDarkplaces7
                     | ProtocolDarkplaces6
                     | ProtocolDarkplaces5
                     | ProtocolDarkplaces4
                     | ProtocolDarkplaces3
                     | ProtocolDarkplaces2
                     | ProtocolDarkplaces1
                     | ProtocolQuakeDP
                     | ProtocolQuake
                     | ProtocolQuakeWorld
                     | ProtocolNehahraMovie
                     | ProtocolNehahraBJP
                     | ProtocolNehahraBJP2
                     | ProtocolNehahraBJP3
    deriving(Show, Eq, Ord, Enum)


data DPServerPacket = DPBad
                    | DPNop
                    | DPDisconnect
                    | DPUpdateStat Word8 Word32 -- Should be signed ?
                    | DPVersion (Maybe ProtocolVersion)
                    | DPSetView Word16
                    | DPSound
                    | DPTime Float
                    | DPPrint L.ByteString
                    | DPStuffText L.ByteString 
                    | DPSetAngle
                    | DPServerInfo (Either Word32 ServerInfoData)
                    | DPLightStyle Word8 L.ByteString
                    | DPUpdateName Word8 L.ByteString  -- <user number> <user name>
                    | DPUpdateFrags Word8 Int16
                    | DPClientData
                    | DPStopSound
                    | DPUpdateColors Word8 Word8 -- <user number> <user color>
                    | DPParticle
                    | DPDamage
                    | DPSpawnStatic
                    | DPSpawnBaseline
                    | DPTempEntity
                    | DPSetPause
                    | DPSignonNum
                    | DPCenterPrint
                    | DPKilledMonster
                    | DPFoundSecret
                    | DPSpawnStaticSound
                    | DPIntermission
                    | DPFinale
                    | DPCDTrack
                    | DPSellScreen
                    | DPCutScene
                    | DPShowlmp
                    | DPHidelmp
                    | DPSkybox -- 37
                    | DPDownloadData Word32 Word16 L.ByteString -- <start> <size> <data> 50
    deriving(Show, Eq)


data ServerInfoData = QWServerInfoData
                    | DPServerInfoData ProtocolVersion Word8 Word8 L.ByteString [L.ByteString] [L.ByteString]
    deriving(Show, Eq)


protocolVersionMaps :: [(Word32, ProtocolVersion, String)]
protocolVersionMaps = [
    (3504, ProtocolDarkplaces7, "DP7"),
	(3503, ProtocolDarkplaces6, "DP6"),
    (3502, ProtocolDarkplaces5, "DP5"),
	(3501, ProtocolDarkplaces4, "DP4"),
	(3500, ProtocolDarkplaces3, "DP3"),
	(97, ProtocolDarkplaces2, "DP2"),
	(96, ProtocolDarkplaces1, "DP1"),
	(15, ProtocolQuakeDP, "QUAKEDP"),
	(15, ProtocolQuake, "QUAKE"),
	(28, ProtocolQuakeWorld, "QW"),
	(250, ProtocolNehahraMovie, "NEHAHRAMOVIE"),
	(10000, ProtocolNehahraBJP, "NEHAHRABJP"),
	(10001, ProtocolNehahraBJP2, "NEHAHRABJP2"),
	(10002, ProtocolNehahraBJP3, "NEHAHRABJP3")]
 

-- get ProtocolVersion by Long Int
protocolVersionFromNum :: Word32 -> Maybe ProtocolVersion
protocolVersionFromNum key = SM.lookup key map_table
  where
    map_table = SM.fromList $ map (\(x, y, z) -> (x, y)) protocolVersionMaps


parsePacket :: Get (Either Word8 DPServerPacket)
parsePacket = sequence =<< getPacketParser <$> getWord8


parsePackets :: Get [Either Word8 DPServerPacket]
parsePackets = do
    empty <- isEmpty
    if empty
        then return []
        else do
            either_packet <- parsePacket
            case either_packet of
                Right packet -> (Right packet:) <$> parsePackets
                Left t -> return [Left t]


getPacketParser :: Word8 -> Either Word8 (Get DPServerPacket)
getPacketParser t = case t of
    0 -> Right parseBad
    1 -> Right parseNop
    2 -> Right parseDisconnect
    3 -> Right parseUpdateStats
    4 -> Right parseVersion
    5 -> Right parseSetView
    -- 6 sound
    7 -> Right parseTime
    8 -> Right parsePrint
    9 -> Right parseStuffText
    11 -> Right parseServerInfo
    12 -> Right parseLightStyle
    13 -> Right parseUpdateName
    14 -> Right parseUpdateFrags
    17 -> Right parseUpdateColors
    50 -> Right parseDownloadData
    _ ->  Left t

parseBad :: Get DPServerPacket
parseBad = return DPBad

parseNop :: Get DPServerPacket
parseNop = return DPNop

parseDisconnect :: Get DPServerPacket
parseDisconnect = return DPDisconnect

parseUpdateStats :: Get DPServerPacket
parseUpdateStats = DPUpdateStat <$> getWord8 <*> getWord32le

parseVersion :: Get DPServerPacket
parseVersion = DPVersion . protocolVersionFromNum <$> getWord32le

parseSetView :: Get DPServerPacket
parseSetView = DPSetView <$> getWord16le

-- parseSound for DPSound

parseTime :: Get DPServerPacket
parseTime = DPTime <$> getFloat32le

parsePrint ::Get DPServerPacket
parsePrint = DPPrint <$> getLazyByteStringNul

parseStuffText :: Get DPServerPacket
parseStuffText = DPStuffText <$> getLazyByteStringNul

-- TODO: not full
parseServerInfo :: Get DPServerPacket
parseServerInfo = do
    proto_num <- getWord32le
    let maybe_proto = protocolVersionFromNum proto_num
    case maybe_proto of
        Nothing -> return $ DPServerInfo (Left proto_num)
        Just proto -> if proto == ProtocolQuakeWorld
            then toDPServerPacket <$> parseQuakeWorldInfo proto
            else toDPServerPacket <$> parseOtherInfo proto
  where
    parseQuakeWorldInfo proto = undefined
    parseOtherInfo proto = do
        maxclients <- getWord8
        gametype <- getWord8
        signon_msg <- getLazyByteStringNul
        models_precached <- getStringList
        sounds_precached <- getStringList
        return $ DPServerInfoData proto maxclients gametype signon_msg models_precached sounds_precached
    toDPServerPacket = DPServerInfo . Right


parseLightStyle :: Get DPServerPacket
parseLightStyle = DPLightStyle <$> getWord8 <*> getLazyByteStringNul


parseUpdateName :: Get DPServerPacket
parseUpdateName = DPUpdateName <$> getWord8 <*> getLazyByteStringNul

parseUpdateFrags :: Get DPServerPacket
parseUpdateFrags = DPUpdateFrags <$> getWord8 <*> (fromIntegral <$> getWord16le)

parseUpdateColors :: Get DPServerPacket
parseUpdateColors = DPUpdateColors <$> getWord8 <*> getWord8


parseDownloadData :: Get DPServerPacket
parseDownloadData = do
    start <- getWord32le
    size <- getWord16le
    download_data <- getLazyByteString $ fromIntegral size
    return $ DPDownloadData start size download_data


getStringList :: Get [L.ByteString]
getStringList = do
    str <- getLazyByteStringNul
    if L.null str
        then return []
        else (str :) <$> getStringList
