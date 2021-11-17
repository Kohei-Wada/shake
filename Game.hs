{-#LANGUAGE RecordWildCards #-}

module Game where

import Snake
import Food
import Types
import Options
import Utils

import Data.Function (fix)
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC
import System.Exit


window :: Display
window = InWindow windowTitle (wWidth, wHeight) (100, 100)


data GameState = InGame | GameOver

data Game = Game
    { _state  :: GameState
    , _food   :: Food
    , _snake  :: Snake
    , _action :: SnakeAction
    , _score  :: Int
    }


initGame :: IO Game
initGame = do
    (target, snakeH) <- fix $ \loop -> do
        g <- createSystemRandom
        target <- randomPosition g
        snakeH <- randomPosition g
        if target == snakeH then loop else pure (target, snakeH)
    pure $ Game InGame target [snakeH] SAStop 0


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = case _state of
    InGame -> do
        let (x, y) = moveSnake _action $ head _snake
            isSelfIntersection = _action /= SAStop && (x, y) `elem` _snake
            snake = (x, y) : _snake

        if isSelfIntersection || x < 0 || x >= cellWidth || y < 0 || y >= cellHeight
            then pure $ g { _state = GameOver }
            else if (x, y) == _food
                then do
                    food <- makeNewFood g
                    pure $ g { _food = food, _snake = snake, _score = _score + 1}
                else pure $ g { _snake = init snake}

    GameOver -> pure g



drawWorld :: Game -> IO Picture
drawWorld Game{..} = case _state of
    InGame -> pure $ pictures
        [ drawCell red _food
        , drawCell (greyN 0.3) (head _snake)
        , pictures $ map (drawCell (greyN 0.6)) (tail _snake)
        , translate (-wWidth/2+10) (-wHeight/2+10)  . scale 0.2 0.2 $ text ("SCORE: " ++ show _score)
        ]
        where
            cell = translate (-wWidth/2) (-wHeight/2) $ 
                polygon [(0, 0), (0, cellSize), (cellSize, cellSize), (cellSize, 0)]

            drawCell c (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ 
                color c cell

    GameOver -> pure $ pictures
        [ translate (-270) 20     . scale 0.7 0.7 $ text "GAME OVER"
        , translate (-100) (-50)  . scale 0.3 0.3 $ text ("SCORE: " ++ show _score)
        , translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
        ]


makeNewFood :: Game -> IO Food
makeNewFood game = do 
    fix $ \loop -> do
        food <- makeRandomFood 
        if food `elem` _snake game  then loop else pure food


eventHandler :: Event -> Game -> IO Game
eventHandler e game@Game{..} = case _state of
    InGame -> case e of
        EventKey (SpecialKey KeyUp)    Down _ _ -> 
            pure $ if _action == SADown  
                      then game 
                      else game { _action = SAUp }

        EventKey (SpecialKey KeyDown)  Down _ _ -> 
            pure $ if _action == SAUp    
                      then game 
                      else game { _action = SADown }

        EventKey (SpecialKey KeyLeft)  Down _ _ -> 
            pure $ if _action == SARight 
                      then game 
                      else game { _action = SALeft }

        EventKey (SpecialKey KeyRight) Down _ _ -> 
            pure $ if _action == SALeft  
                      then game 
                      else game { _action = SARight }

        EventKey (Char 'q') Down _ _ -> 
            exitSuccess

        _ ->
            pure game 

    GameOver -> case e of
        EventKey (Char 'q') Down _ _ -> 
            exitSuccess

        EventKey (SpecialKey KeyEnter) Down _ _ -> 
            initGame

        _ ->
            pure game 


gameMain :: IO ()
gameMain = do
    game <- initGame
    playIO window white 10 game drawWorld eventHandler updateGame



