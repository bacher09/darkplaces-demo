module DarkPlaces.Types (
    QVector(),
    ClientStatsEnum(..),
    ClientStatsList,
    ProtocolVersion(..),
    GameMode(..),
    consQVector,
    qvectorFromList,
    protocolVersionFromNum,
    statsFromNum
) where

import qualified Data.Map.Strict as SM
import Data.Word


newtype QVector = QVector (Float, Float, Float) deriving (Show, Eq, Ord)


data ClientStatsEnum = HealthStat
                     | WeaponStat
                     | AmmoStat
                     | ArmorStat
                     | WeaponFrameStat
                     | ShellsStat
                     | NailsStat
                     | RocketsStat
                     | CellsStat
                     | ActiveWeaponStat
                     | TotalSecretsStat
                     | TotalMonstersStat
                     | SecretsStat
                     | MonstersStat
                     | ItemsStat
                     | ViewHeightStat
                     | ViewZoomStat
    deriving(Show, Eq, Enum, Bounded)


type ClientStatsList = [(ClientStatsEnum, Int)]


data ProtocolVersion = ProtocolQuake
                     | ProtocolQuakeWorld
                     | ProtocolQuakeDP
                     | ProtocolNehahraMovie
                     | ProtocolNehahraBJP
                     | ProtocolNehahraBJP2
                     | ProtocolNehahraBJP3
                     | ProtocolDarkplaces1
                     | ProtocolDarkplaces2
                     | ProtocolDarkplaces3
                     | ProtocolDarkplaces4
                     | ProtocolDarkplaces5
                     | ProtocolDarkplaces6
                     | ProtocolDarkplaces7
    deriving(Show, Eq, Ord, Bounded, Enum)


data GameMode = GameNormal
              | GameHipnotic
              | GameRogue
              | GameQuoth
              | GameNehahra
              | GameNexuiz
              | GameXonotic
              | GameTransfusion
              | GameGoodVsBad2
              | GameTeu
              | GameBattlemech
              | GameZymotic
              | GameSetheral
              | GameTenebrae
              | GameNeoteric
              | GameOpenquartz
              | GamePrydon
              | GameDeluxeQuake
              | GameThehunTed
              | GameDefeatIndetail2
              | GameDarsana
              | GameContagionTheory
              | GameEdu2p
              | GameProphecy
              | GameBloodOmnicide
              | GameSteelStorm
              | GameSteelStorm2
              | GameSsammo
              | GameTomesofMephistopheles
              | GameStrapBomb
              | GameMoonhelm
              | GameVoreTournament
              | GameCount
    deriving(Show, Eq, Enum, Bounded)


consQVector :: Float -> Float -> Float -> QVector
consQVector x y z = QVector (x, y, z)

qvectorFromList :: [Float] -> Maybe QVector
qvectorFromList arg = case arg of
    [x, y, z] -> Just $ QVector (x, y, z)
    _          -> Nothing


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
protocolVersionFromNum :: (Integral a) => a -> Maybe ProtocolVersion
protocolVersionFromNum key = SM.lookup (fromIntegral key) map_table
  where
    map_table = SM.fromList $ map (\(x, y, _) -> (x, y)) protocolVersionMaps


clientStatsMap :: [(Word8, ClientStatsEnum)]
clientStatsMap = [(0, HealthStat), (2, WeaponStat), (3, AmmoStat),
                  (4, ArmorStat), (5, WeaponFrameStat), (6, ShellsStat),
                  (7, NailsStat), (8, RocketsStat), (9, CellsStat),
                  (10, ActiveWeaponStat), (11, TotalSecretsStat), (12, TotalMonstersStat),
                  (13, SecretsStat), (14, MonstersStat),(15, ItemsStat),
                  (16, ViewHeightStat), (21, ViewZoomStat)]


statsFromNum :: (Integral a) => a -> Maybe ClientStatsEnum
statsFromNum a = SM.lookup (fromIntegral a) search_map
  where
    search_map = SM.fromList clientStatsMap
