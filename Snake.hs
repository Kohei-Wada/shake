{-#LANGUAGE RecordWildCards #-}
module Snake where

import Types
import Utils
import System.Random.MWC

data Snake = Snake 
    {
    _ls :: [Position]
    }

data SnakeAction 
    = SAStop 
    | SAUp 
    | SADown 
    | SALeft 
    | SARight 
    deriving Eq



snakeHead :: Snake -> Position 
snakeHead s@Snake{..} = head _ls

snakeTail :: Snake -> [Position]
snakeTail s@Snake{..} = tail _ls

snakeInit :: Snake -> [Position]
snakeInit s@Snake{..} = init _ls


snakeHeadX :: Snake -> Int
snakeHeadX s@Snake{..} = fst $ head _ls

snakeHeadY :: Snake -> Int
snakeHeadY s@Snake{..} = snd $ head _ls


snakeInterference :: Snake -> Position -> Bool
snakeInterference s p = p `elem` _ls s


prevSnake :: Snake -> Snake
prevSnake s = Snake $ init $ _ls s

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
initSnake = createSystemRandom >>= randomPosition >>= \p -> return $ Snake [p]


updateSnakeAction :: Snake -> SnakeAction -> Snake
updateSnakeAction s a = s


updateSnake :: Snake -> SnakeAction -> Snake
updateSnake s a =  Snake $ (moveSnake a (snakeHead s)) : _ls s


--TODO  separate snake action from game modlue
selfIntersection :: Snake -> SnakeAction -> Bool
selfIntersection s SAStop = False
selfIntersection s _ = head (_ls s) `elem` (tail) (_ls s)




