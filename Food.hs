module Food where

import Utils
import Types
import Options


import System.Random.MWC

type Food = Position


makeRandomFood :: IO Food
makeRandomFood = do 
    createSystemRandom >>= randomPosition >>= return




