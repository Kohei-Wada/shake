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
    p <- randomPosition 
    return $ Food p


randomFoods :: Int -> IO [Food]
randomFoods 0 = return []
randomFoods n = do 
    fs <- randomFoods (n - 1) 
    f  <- fix $ \loop -> do 
        f' <- randomFood 
        if any (== f') fs then loop else return f'
    return (f : fs)


rmFoodByPos :: [Food] -> Position -> [Food]
rmFoodByPos fs p = filter (\f -> _pos f /= p) fs



