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


invAction :: SnakeAction -> SnakeAction 
invAction SAUp    = SADown
invAction SADown  = SAUp
invAction SARight = SALeft
invAction SALeft  = SARight


moveSnake :: SnakeAction -> Position -> Position
moveSnake SAStop  (x, y) = (x, y)
moveSnake SAUp    (x, y) = (x, y + 1)
moveSnake SADown  (x, y) = (x, y - 1)
moveSnake SALeft  (x, y) = (x - 1, y)
moveSnake SARight (x, y) = (x + 1, y)


initSnake :: IO Snake
initSnake = createSystemRandom >>= randomPosition >>= \p -> return $ [p]


updateSnakeAction :: Snake -> SnakeAction -> Snake
updateSnakeAction s a = s


updateSnake :: Snake -> SnakeAction -> Snake
updateSnake s a =  (moveSnake a $ head s) : s


--TODO  separate snake action from game modlue
selfIntersection :: Snake -> SnakeAction -> Bool
selfIntersection s SAStop = False
selfIntersection s _ = head s `elem` (tail) s




