module Simulation where

import Relude

import SimulationTypes

import Web.Scotty.Trans
import qualified Data.Text.Lazy             as T
import GHC.Float (int2Double)

minAge :: Sex -> Int
minAge Male = 65
minAge Female = 63

-- average time to death starting from min retirement age
avgLifespan :: Sex -> Int
avgLifespan sex = let ys = 76 - minAge sex in 12 * ys

-- poart of salary that goes to ZUS
ratio :: Double
ratio = 0.172

-- revaluation ratio
cpiRatio :: Double
cpiRatio = 1.05

calc :: Double -> Double
calc salary = salary * ratio

simulate :: Sex -> Double -> Int -> Int -> Double
simulate sex salary start end = foldl' f 0 parts / int2Double (avgLifespan sex)
    where
        f acc x = (acc + x) * cpiRatio
        allYears = if end > start then end - start else 0
        parts = replicate allYears $ calc (salary*12)
