module Utils where

import Types
import Options
import System.Random.MWC


randomPosition :: IO Position
randomPosition = do 
    gen <- createSystemRandom
    (,) <$> uniformR (0, cellWidth  - 1) gen 
        <*> uniformR (0, cellHeight - 1) gen


