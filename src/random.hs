module Random where

import Control.Monad.Random (getRandomR)
import System.Random 
import qualified Control.Monad.Random

type Rand = Control.Monad.Random.Rand StdGen

-- Creates a random value in the range [0.0, 1.0]
randFloat :: () -> Rand Float
randFloat () = getRandomR (0.0, 1.0)

-- Creates a random integer in the range [0, max)
randInt :: Int -> Rand Int
randInt maxVal = getRandomR (0, maxVal - 1)

