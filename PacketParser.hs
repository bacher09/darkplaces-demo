module PacketParser (
    DPServerPacket(..),
    parsePacket
) where

import Prelude hiding (sequence)
import Control.Monad hiding (sequence, mapM)
import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
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
                    | DPUpdateStat Word8 Word8
                    | DPVersion (Maybe ProtocolVersion)
                    | DPSetView Word16
                    | DPSound
                    | DPTime Float
                    | DPPrint L.ByteString
                    | DPStuffText L.ByteString 
                    | DPSetAngle
                    | DPServerInfo
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


parsePacket :: Get (Maybe DPServerPacket)
parsePacket = sequence =<< getPacketParser <$> getWord8


getPacketParser :: Word8 -> Maybe (Get DPServerPacket)
getPacketParser t = case t of
    0 -> Just parseBad
    1 -> Just parseNop
    2 -> Just parseDisconnect
    3 -> Just parseUpdateStats
    4 -> Just parseVersion
    5 -> Just parseSetView
    -- 6 sound
    7 -> Just parseTime
    8 -> Just parsePrint
    9 -> Just parseStuffText
    _ -> Nothing

parseBad :: Get DPServerPacket
parseBad = return DPBad

parseNop :: Get DPServerPacket
parseNop = return DPNop

parseDisconnect :: Get DPServerPacket
parseDisconnect = return DPDisconnect

parseUpdateStats :: Get DPServerPacket
parseUpdateStats = DPUpdateStat <$> getWord8 <*> getWord8

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
