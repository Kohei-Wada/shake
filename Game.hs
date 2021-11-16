{-#LANGUAGE RecordWildCards #-}

module Game where

import Snake

import Data.Function (fix)
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC


wWidth, wHeight :: Num a => a
wWidth  = 640
wHeight = 480


window :: Display
window = InWindow "Snake Game" (wWidth, wHeight) (100, 100)


cSize, cWidth, cHeight :: Num a => a
cSize   = 20
cWidth  = fromIntegral $ wWidth  `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize



randomPosition :: GenIO -> IO Position
randomPosition gen = (,) <$> uniformR (0, cWidth - 1) gen <*> uniformR (0, cHeight - 1) gen

data GameState = InGame | GameOver


data World = World
    { _state  :: GameState
    , _target :: Position
    , _snake  :: [Position]
    , _action :: SnakeAction
    , _score  :: Int
    }


generateNewWorld :: IO World
generateNewWorld = do
    (target, snakeH) <- withSystemRandom . asGenIO $ \gen -> do
        fix $ \loop -> do
            target <- randomPosition gen
            snakeH <- randomPosition gen
            if target == snakeH then loop else pure (target, snakeH)
    pure $ World InGame target [snakeH] SAStop 0



drawWorld :: World -> IO Picture
drawWorld World{..} = case _state of
    InGame -> pure $ pictures
        [ drawCell red _target
        , drawCell (greyN 0.3) (head _snake)
        , pictures $ map (drawCell (greyN 0.6)) (tail _snake)
        , translate (-wWidth/2+10) (-wHeight/2+10)  . scale 0.2 0.2 $ text ("SCORE: " ++ show _score)
        ]
        where
            cell = translate (-wWidth/2) (-wHeight/2) $ 
                polygon [(0, 0), (0, cSize), (cSize, cSize), (cSize, 0)]

            drawCell c (x, y) = translate (fromIntegral x * cSize) (fromIntegral y * cSize) $ 
                color c cell

    GameOver -> pure $ pictures
        [ translate (-270) 20     . scale 0.7 0.7 $ text "GAME OVER"
        , translate (-100) (-50)  . scale 0.3 0.3 $ text ("SCORE: " ++ show _score)
        , translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
        ]


eventHandler :: Event -> World -> IO World
eventHandler e game@World{..} = case _state of
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
        _ ->
            pure game 

    GameOver -> case e of
        EventKey (SpecialKey KeyEnter) Down _ _ -> 
            generateNewWorld

        _ ->
            pure game 


updateGame :: Float -> World -> IO World
updateGame _ w@World{..} = case _state of
    InGame -> do
        let (x, y) = moveSnake _action $ head _snake
            isSelfIntersection = _action /= SAStop && (x, y) `elem` _snake
            snake = (x, y) : _snake

        if isSelfIntersection || x < 0 || x >= cWidth || y < 0 || y >= cHeight
            then pure $ w { _state = GameOver }
            else if (x, y) == _target
                then do
                    target <- withSystemRandom . asGenIO $ \gen -> do
                        fix $ \loop -> do
                            target <- randomPosition gen
                            if target `elem` snake then loop else pure target
                    pure $ w { _target = target, _snake = snake, _score = _score + 1}
                else pure $ w { _snake = init snake}

    GameOver -> pure w


gameInit :: IO ()
gameInit = do
    world <- generateNewWorld
    playIO window white 10 world drawWorld eventHandler updateGame

