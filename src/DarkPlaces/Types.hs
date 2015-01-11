module DarkPlaces.Types (
    QVector(),
    ClientStatsEnum(..),
    ClientStatsList(..),
    GameMode(..),
    getQVector,
    consQVector,
    qvectorFromList
) where

import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754

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

getQVector :: Get QVector
getQVector = consQVector <$> getFloat32le <*> getFloat32le <*> getFloat32le


qvectorFromList :: [Float] -> Maybe QVector
qvectorFromList arg = case arg of
    [x, y, z] -> Just $ QVector (x, y, z)
    _          -> Nothing
