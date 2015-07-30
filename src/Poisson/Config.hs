module Poisson.Config where

import Util.Geometry

-- Stores relevant options for poisson-disc sampling
data Config = Config {
    topLeft         :: Vector,
    lowerRight      :: Vector,
    center          :: Vector,
    dimensions      :: Vector,
    minDist         :: Float,
    cellSize        :: Float,
    ptsPerIter      :: Int,
    gridWidth       :: Int,
    gridHeight      :: Int 
} deriving (Show)

mkConfig :: Vector -> Vector -> Float -> Int -> Config
mkConfig tl lr minimumDist perIter =
    let dim@(Vector dimX dimY)= lr `vSub` tl
        cSize = minimumDist / sqrt 2.0 in
            Config { 
                topLeft = tl,
                lowerRight = lr,
                center = tl `vAdd` scaleUniform dim 0.5,
                dimensions = dim,
                minDist = minimumDist,
                cellSize = cSize,
                ptsPerIter = perIter,
                gridWidth = ceiling (dimX / cSize),
                gridHeight = ceiling (dimY / cSize) 
            }

-- Creates a configuration designed to sample points in an image
-- with given dimensions, where distance is defined in pixels
mkConfigForImg :: (Int, Int) -> Float -> Int -> Config
mkConfigForImg (width, height) = mkConfig (Vector 0 0) (Vector w h) 
    where w = fromIntegral width
          h = fromIntegral height
