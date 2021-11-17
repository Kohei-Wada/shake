{-#LANGUAGE RecordWildCards #-}

module Game where

import Snake
import Data.Function (fix)
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC
import System.Exit


wWidth, wHeight :: Num a => a
wWidth  = 840
wHeight = 480


window :: Display
window = InWindow "Snake Game" (wWidth, wHeight) (100, 100)


cSize, cWidth, cHeight :: Num a => a
cSize   = 20
cWidth  = fromIntegral $ wWidth  `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize


randomPosition :: GenIO -> IO Position
randomPosition gen = (,) <$> uniformR (0, cWidth - 1) gen <*> uniformR (0, cHeight - 1) gen


type Food = Position
data GameState = InGame | GameOver

data World = World
    { _state  :: GameState
    , _food   :: Food
    , _snake  :: Snake
    , _action :: SnakeAction
    , _score  :: Int
    }


initGame :: IO World
initGame = do
    (target, snakeH) <- fix $ \loop -> do
        g <- createSystemRandom
        target <- randomPosition g
        snakeH <- randomPosition g
        if target == snakeH then loop else pure (target, snakeH)
    pure $ World InGame target [snakeH] SAStop 0


drawWorld :: World -> IO Picture
drawWorld World{..} = case _state of
    InGame -> pure $ pictures
        [ drawCell red _food
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


makeNewTarget :: World -> IO Position
makeNewTarget game  = do
    fix $ \loop -> do
        g <- createSystemRandom
        target <- randomPosition g 
        if target `elem` _snake game  then loop else pure target


updateGame :: Float -> World -> IO World
updateGame _ w@World{..} = case _state of
    InGame -> do
        let (x, y) = moveSnake _action $ head _snake
            isSelfIntersection = _action /= SAStop && (x, y) `elem` _snake
            snake = (x, y) : _snake

        if isSelfIntersection || x < 0 || x >= cWidth || y < 0 || y >= cHeight
            then pure $ w { _state = GameOver }
            else if (x, y) == _food
                then do
                    target <- makeNewTarget w
                    pure $ w { _food = target, _snake = snake, _score = _score + 1}
                else pure $ w { _snake = init snake}

    GameOver -> pure w



gameMain :: IO ()
gameMain = do
    world <- initGame
    playIO window white 10 world drawWorld eventHandler updateGame

