module Food where

import Utils
import Types
import Options

import System.Random.MWC
import Data.Function (fix)


data Food = Food 
    { _pos :: Position
    } deriving Eq


randomFood :: IO Food
randomFood = do 
    p <- createSystemRandom >>= randomPosition 
    return $ Food p


randomFoods :: Int -> IO [Food]
randomFoods 0 = randomFood >>= \f -> return [f]
randomFoods n = do 
    fs <- randomFoods (n - 1) 
    f  <- fix $ \loop -> do 
        f' <- randomFood 
        if any (== f') fs then loop else return f'
    return (f : fs)


obtainFood :: [Food] -> Position -> Maybe Food
obtainFood [] _     = Nothing
obtainFood (f:fs) p = if _pos f == p then Just f else obtainFood fs p



rmFood :: [Food] -> Food -> [Food]
rmFood fs f = filter (==f) fs


