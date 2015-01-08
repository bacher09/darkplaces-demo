module DPTypes (
    QVector(),
    getQVector
) where

import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754

newtype QVector = QVector (Float, Float, Float) deriving (Show, Eq)

getQVector :: Get QVector
getQVector = (\x y z -> QVector (x, y, z)) <$> getFloat32le <*> getFloat32le <*> getFloat32le
