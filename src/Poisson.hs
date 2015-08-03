module Poisson (
    mkConfig,
    mkConfigForImg,
    generatePoints
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Array.IArray
import Data.Map (Map)
import Generics.Pointless.Combinators (andf)
import qualified Data.Map.Strict as Map

import Poisson.Config
import Util.Geometry
import Util.Random

type Point = Vector
type GridIndex = (Int, Int)
type GridValue = Maybe Vector
type Grid = Array GridIndex GridValue 
type ActiveMap = Map Vector Bool

-- Contains relevant information about a grid at a given time during sampling
data GridState = GridState Grid ActiveMap deriving (Show)

-- Initialize an empty grid
initGrid :: Int -> Int -> Grid
initGrid width height = array ((0, 0), (iMax, jMax)) initVal
    where iMax = width - 1
          jMax = height - 1
          initVal = [((i, j), Nothing) | i <- [0..iMax], j <- [0..jMax]]

-- Gets the grid index which contains the given point
discretize :: Config -> Point -> GridIndex 
discretize cfg p = mapTuple truncate (xCoord, yCoord)
    where mapTuple f (a, b) = (f a, f b)
          origin = topLeft cfg
          cSize = cellSize cfg
          (Vector xCoord yCoord) = (p `vSub` origin) `scaleUniform` (1.0 / cSize)

-- Adds a point to the given grid state
addPoint :: Config -> GridState -> Point -> GridState
addPoint cfg (GridState grid activeMap) p = GridState newGrid newActiveMap where
    pIndex = discretize cfg p 
    newGrid = grid // [(pIndex, Just p)]
    newActiveMap = Map.insert p True activeMap

-- Toggles the active status of a given base point, returning the updated GridState
toggleActive :: GridState -> Point -> GridState
toggleActive (GridState grid activeMap) basePoint = GridState grid updatedMap
    where updatedMap = Map.adjust not basePoint activeMap

-- Ensures a point is within the boundaries set by a given configuration
inBounds :: Config -> Point -> Bool
inBounds cfg (Vector x y) = minX <= x && x < maxX && minY <= y && y < maxY
    where (Vector minX minY) = topLeft cfg
          (Vector maxX maxY) = lowerRight cfg

-- Creates a grid state with an initial random point
initGridState :: Config -> Rand GridState
initGridState cfg = do
    randXY <- (,) <$> randFloat () <*> randFloat () 
    let offset = scale (dimensions cfg) randXY 
        p = topLeft cfg `vAdd` offset
        grid = initGrid (gridWidth cfg) (gridHeight cfg) 
        gState = GridState grid Map.empty
    if inBounds cfg p then return $! addPoint cfg gState p
    else initGridState cfg 

-- Checks if a point is too close any other point on the grid
tooClose :: Config -> (GridIndex -> GridValue) -> Point -> Bool
tooClose cfg getGridVal p = any isTooClose gridVals where
    (pxIndex, pyIndex) = discretize cfg p 
    lowX = max 0 (pxIndex - 2)
    lowY = max 0 (pyIndex - 2)
    highX = min (gridWidth cfg) (pxIndex + 3) - 1
    highY = min (gridHeight cfg) (pyIndex + 3) - 1 
    gridVals = [getGridVal (x, y) | x <- [lowX .. highX], y <- [lowY .. highY]] 
    isTooClose (Just gridVal) = gridVal `dist` p < minDist cfg
    isTooClose Nothing        = False 

-- Creates a random offset and adds it to a given base point
makeRandomPoint :: Config -> Point -> Rand Point
makeRandomPoint cfg basePoint = do
    rScale <- randFloat ()
    tScale <- randFloat ()
    let minD = minDist cfg
        radius = minD + minD * rScale
        theta = 2.0 * pi * tScale
        offset = Vector (radius * sin theta) (radius * cos theta) 
    return $ basePoint `vAdd` offset

-- Picks a base point for the next set of random samples
pickBasePoint :: [Point] -> Rand Point
pickBasePoint activePoints = liftM (activePoints !!) (randInt $ length activePoints) 

-- Samples a single point, and updates the grid state if the point is valid
sampleBasePointOnce :: Config -> Point -> GridState -> Rand GridState
sampleBasePointOnce cfg basePoint gState@(GridState grid _) = do
    let getGridVal = (grid !)
        isValidPoint = andf (inBounds cfg, not . tooClose cfg getGridVal)
    sample <- makeRandomPoint cfg basePoint
    if isValidPoint $ sample then return $! addPoint cfg gState sample
    else return gState

-- Repeatedly samples points from a given base point, updating
-- the grid state incrementally
sampleBasePoint :: Config -> Point -> GridState -> Rand GridState
sampleBasePoint cfg basePoint = foldr (<=<) return $ replicate n doSample
    where n = ptsPerIter cfg
          doSample = sampleBasePointOnce cfg basePoint

-- Samples a given grid state repeatedly, deactivating base points
-- which fail to yield any new samples
sample :: Config -> Rand GridState -> Rand GridState
sample cfg gridState = do
    gState@(GridState _ activeMap) <- gridState
    let activePoints = map fst . filter snd . Map.assocs $ activeMap 
    if null activePoints then gridState
    else do
        basePoint <- pickBasePoint activePoints 
        newGState@(GridState _ newActiveMap) <- sampleBasePoint cfg basePoint gState
        if activeMap == newActiveMap then
            sample cfg . return $! toggleActive newGState basePoint
        else sample cfg . return $! newGState

-- Generate a set of random coordinate points using the Poisson-disc sampling algorithm
generatePoints :: Config -> Rand [Vector]
generatePoints cfg = liftM getPts grid
    where grid = sample cfg $ initGridState cfg
          getPts (GridState _ activeMap) = map fst . Map.assocs $ activeMap
