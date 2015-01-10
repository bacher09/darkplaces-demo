module DPTypes (
    QVector(),
    ClientStatsEnum(..),
    ClientStatsList(..),
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

{-MOVEVARS_AIRACCEL_QW_STRETCHFACTOR-}
{-MOVEVARS_AIRCONTROL_PENALTY-}
{-MOVEVARS_AIRSPEEDLIMIT_NONQW-}
{-MOVEVARS_AIRSTRAFEACCEL_QW-}
{-MOVEVARS_AIRCONTROL_POWER-}
{-MOVEFLAGS-}
{-MOVEVARS_WARSOWBUNNY_AIRFORWARDACCEL-}
{-MOVEVARS_WARSOWBUNNY_ACCEL-}
{-MOVEVARS_WARSOWBUNNY_TOPSPEED-}
{-MOVEVARS_WARSOWBUNNY_TURNACCEL-}
{-MOVEVARS_WARSOWBUNNY_BACKTOSIDERATIO-}
{-MOVEVARS_AIRSTOPACCELERATE-}
{-MOVEVARS_AIRSTRAFEACCELERATE-}
{-MOVEVARS_MAXAIRSTRAFESPEED-}
{-MOVEVARS_AIRCONTROL-}
{-FRAGLIMIT-}
{-TIMELIMIT-}
{-MOVEVARS_WALLFRICTION-}
{-MOVEVARS_FRICTION-}
{-MOVEVARS_WATERFRICTION-}
{-MOVEVARS_TICRATE-}
{-MOVEVARS_TIMESCALE-}
{-MOVEVARS_GRAVITY-}
{-MOVEVARS_STOPSPEED-}
{-MOVEVARS_MAXSPEED-}
{-MOVEVARS_SPECTATORMAXSPEED-}
{-MOVEVARS_ACCELERATE-}
{-MOVEVARS_AIRACCELERATE-}
{-MOVEVARS_WATERACCELERATE-}
{-MOVEVARS_ENTGRAVITY-}
{-MOVEVARS_JUMPVELOCITY-}
{-MOVEVARS_EDGEFRICTION-}
{-MOVEVARS_MAXAIRSPEED-}
{-MOVEVARS_STEPHEIGHT-}


consQVector :: Float -> Float -> Float -> QVector
consQVector x y z = QVector (x, y, z)

getQVector :: Get QVector
getQVector = consQVector <$> getFloat32le <*> getFloat32le <*> getFloat32le


qvectorFromList :: [Float] -> Maybe QVector
qvectorFromList arg = case arg of
    (x:y:z:[]) -> Just $ QVector (x, y, z)
    _          -> Nothing
