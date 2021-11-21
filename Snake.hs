{-# LANGUAGE RecordWildCards #-}
module Snake where

import Types
import Utils
import System.Random.MWC

data Snake = Snake 
    { _ls     :: [Position]
    , _action :: SnakeAction
    }

data SnakeAction 
    = SAStop 
    | SAUp 
    | SADown 
    | SALeft 
    | SARight 
    | SANone  -- Never used
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
prevSnake s@Snake{..} = s{ _ls = init _ls }


invAction :: SnakeAction -> SnakeAction 
invAction SAStop  = SANone
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
initSnake = randomPosition >>= \p -> return $ Snake [p] SAStop


updateSnakeAction :: Snake -> SnakeAction -> Snake
updateSnakeAction s@Snake{..} a = 
        if a /= invAction _action then s { _action = a } else s


-- Warning : It is wasteful because it reserves memory even when the snake is stationary.
updateSnake :: Snake -> Snake
updateSnake s@Snake{..} = 
    let newHead = moveSnake _action (snakeHead s) in s { _ls = newHead : _ls }


selfIntersection :: Snake -> Bool
selfIntersection s@Snake{..} = 
    if _action == SAStop then False else head _ls `elem` tail _ls
                                  

