module Snake where

import Types
import Utils
import System.Random.MWC

type Snake = [Position]

data SnakeAction 
    = SAStop 
    | SAUp 
    | SADown 
    | SALeft 
    | SARight 
    deriving Eq


moveSnake :: SnakeAction -> Position -> Position
moveSnake SAStop  (x, y) = (x, y)
moveSnake SAUp    (x, y) = (x, y + 1)
moveSnake SADown  (x, y) = (x, y - 1)
moveSnake SALeft  (x, y) = (x - 1, y)
moveSnake SARight (x, y) = (x + 1, y)


initSnake :: IO Snake
initSnake = do 
    createSystemRandom >>= randomPosition >>= \p -> return $ [p]


updateSnakeAction :: Snake -> SnakeAction -> Snake
updateSnakeAction s a = s


updateSnake :: Snake -> SnakeAction -> Snake
updateSnake s a = let nextHead = moveSnake a $ head s in nextHead : s


selfIntersection :: Snake -> SnakeAction -> Bool
selfIntersection s a = let tmp = moveSnake a $ head s in tmp `elem` tail s 

