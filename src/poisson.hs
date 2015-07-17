module Poisson (
    Configuration,
    makeConfig,
    generatePoints
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Array.IArray
import Data.Map (Map)
import Generics.Pointless.Combinators (andf)
import qualified Data.Map.Strict as Map

import Geometry
import Random

data Configuration = Configuration {
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

makeConfig :: Vector -> Vector -> Float -> Int -> Configuration
makeConfig tl lr minimumDist perIter =
    let dim@(Vector dimX dimY)= lr `vSub` tl
        cSize = minimumDist / sqrt 2.0 in 
            Configuration { 
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

type GridIndex = (Int, Int)
type GridValue = Maybe Vector
type Grid = Array GridIndex GridValue 

initGrid :: Int -> Int -> Grid
initGrid width height = array ((0, 0), (iMax, jMax)) initVal
    where iMax = width - 1
          jMax = height - 1
          initVal = [((i, j), Nothing) | i <- [0..iMax], j <- [0..jMax]]

type ActiveMap = Map Vector Bool
data GridState = GridState Grid ActiveMap deriving (Show)

denormalize :: Configuration -> Vector -> GridIndex 
denormalize cfg p = mapTuple truncate (xCoord, yCoord)
    where mapTuple f (a, b) = (f a, f b)
          origin = topLeft cfg
          cSize = cellSize cfg
          (Vector xCoord yCoord) = (p `vSub` origin) `scaleUniform` (1.0 / cSize)

-- Adds a point to the given gridstate
addPoint :: Configuration -> GridState -> Vector -> GridState
addPoint cfg (GridState grid activeMap) p = GridState newGrid newActiveMap where
    pIndex = denormalize cfg p 
    newGrid = grid // [(pIndex, Just p)]
    newActiveMap = Map.insert p True activeMap

-- Toggles the active status of a given base point, returning the updated GridState
toggleActive :: GridState -> Vector -> GridState
toggleActive (GridState grid activeMap) basePoint = GridState grid updatedMap
    where updatedMap = Map.adjust not basePoint activeMap

-- Ensures a point is within the boundaries set by a given configuration
inBounds :: Configuration -> Vector -> Bool
inBounds cfg (Vector x y) = minX <= x && x < maxX && minY <= y && y < maxY
    where (Vector minX minY) = topLeft cfg
          (Vector maxX maxY) = lowerRight cfg

-- Creates a grid state with an initial random point
initGridState :: Configuration -> Rand GridState
initGridState cfg = do
    randXY <- (,) <$> randFloat () <*> randFloat () 
    let offset = scale (dimensions cfg) randXY 
        p = topLeft cfg `vAdd` offset
        initCoord = denormalize cfg p 
        activeMap = Map.singleton p True
        grid = initGrid (gridWidth cfg) (gridHeight cfg) // [(initCoord, Just p)]
    if inBounds cfg p then return $ GridState grid activeMap 
    else initGridState cfg 

-- Checks if a point is too close any other point on the grid
tooClose :: Configuration -> (GridIndex -> GridValue) -> Vector -> Bool
tooClose cfg getGridVal p = any isTooClose gridVals where
    (pxIndex, pyIndex) = denormalize cfg p 
    lowX = max 0 (pxIndex - 2)
    lowY = max 0 (pyIndex - 2)
    highX = min (gridWidth cfg) (pxIndex + 3) - 1
    highY = min (gridHeight cfg) (pyIndex + 3) - 1 
    gridVals = [getGridVal (x, y) | x <- [lowX .. highX], y <- [lowY .. highY]] 
    isTooClose (Just gridVal) = gridVal `dist` p < minDist cfg
    isTooClose Nothing        = False 

-- Creates a random offset and adds it to a given base point
makeRandomPoint :: Configuration -> Vector -> Rand Vector
makeRandomPoint cfg basePoint = do
    rScale <- randFloat ()
    tScale <- randFloat ()
    let minD = minDist cfg
        radius = minD + minD * rScale
        theta = 2.0 * pi * tScale
        offset = Vector (radius * sin theta) (radius * cos theta) 
    return $ basePoint `vAdd` offset

-- Picks a base point for the next set of random samples
pickBasePoint :: [Vector] -> Rand Vector
pickBasePoint activePoints = liftM (activePoints !!) (randInt $ length activePoints) 

-- Returns all valid random points sampled from the given base point
sampleFromBasePoint :: Configuration -> GridState -> Vector -> Rand [Vector]
sampleFromBasePoint cfg (GridState grid _) basePoint = liftM (filter isValidPoint) samples where
    getGridVal = (grid !)
    isValidPoint = andf (inBounds cfg, not . tooClose cfg getGridVal)
    samples = replicateM (ptsPerIter cfg) (makeRandomPoint cfg basePoint)

sample :: Configuration -> Rand GridState -> Rand GridState
sample cfg gridState = do
    gState@(GridState _ activeMap) <- gridState
    let activePoints = map fst . filter snd . Map.assocs $ activeMap 
    if null activePoints then gridState
    else do
        basePoint <- pickBasePoint activePoints
        samples <- sampleFromBasePoint cfg gState basePoint 
        if null samples then sample cfg . return $ toggleActive gState basePoint
        else sample cfg . return $ foldl (addPoint cfg) gState samples

generatePoints :: Configuration -> Rand GridState
generatePoints cfg = sample cfg . initGridState $ cfg
