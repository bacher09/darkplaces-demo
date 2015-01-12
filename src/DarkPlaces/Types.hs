module DarkPlaces.Types (
    QVector(),
    ClientStatsEnum(..),
    ClientStatsList(..),
    GameMode(..),
    consQVector,
    qvectorFromList,
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
