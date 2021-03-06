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


data GameState = InGame | GameOver


data Game = Game
    { _state  :: GameState
    , _foods  :: [Food]
    , _snake  :: Snake
    , _score  :: Int
    }


makeNewFood :: Game -> IO Food
makeNewFood game@Game{..} = fix $ \loop -> do
        f <- randomFood 
        if snakeInterference _snake (_pos f) || any (== f) _foods 
           then loop else pure f


isPossibleSnake :: Snake -> Bool
isPossibleSnake s = x < 0 || x >= cellWidth || y < 0 || y >= cellHeight
    where (x, y) = snakeHead s


initGame :: IO Game
initGame = do 
    fs <- randomFoods nFoods 
    s  <- fix $ \loop -> do 
        s' <- initSnake
        if any ((== snakeHead s') . _pos ) fs then loop else pure s'
    pure $ Game InGame fs s 0


eatFood :: Snake -> Game -> Bool
eatFood s Game{..} = let h = snakeHead s in any ((==h) . _pos) _foods


rmFood :: Snake -> Game -> [Food]
rmFood s Game{..} = let h = snakeHead s in rmFoodByPos _foods h 


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = case _state of 
    InGame -> do 
        let s = updateSnake _snake 
        if isPossibleSnake s || selfIntersection s
           then pure $ g { _state = GameOver } 
           else if eatFood s g
                then do 
                     f <- makeNewFood g 
                     pure $ g { _foods = f : rmFood s g, _snake = s, _score = _score + 1 }
                else
                     pure $ g { _snake = prevSnake s } 

    GameOver -> pure g


drawWorld :: Game -> IO Picture
drawWorld Game{..} = case _state of
    InGame -> pure $ pictures
        [ pictures $ map ((drawCell foodColor) . _pos) _foods
        , drawCell snakeHeadColor (snakeHead _snake)
        , pictures $ map (drawCell snakeTailColor) (snakeTail _snake)
        , translate (-wWidth / 2 + 10) (-wHeight / 2 + 10) . scale 0.2 0.2 $ 
            text ("SCORE: " ++ show _score)
        ]

        where
            cell = translate (-wWidth / 2) (-wHeight / 2) $ 
                polygon [(0, 0), (0, cellSize), (cellSize, cellSize), (cellSize, 0)]

            drawCell c (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ 
                color c cell

    GameOver -> pure $ pictures
        [ translate (-270) 20     . scale 0.7 0.7 $ text "GAME OVER!"
        , translate (-100) (-50)  . scale 0.3 0.3 $ text ("SCORE: " ++ show _score)
        , translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
        ]


eventHandler :: Event -> Game -> IO Game
eventHandler e g@Game{..} = case _state of
    InGame -> case e of

        EventKey (SpecialKey KeyUp) Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SAUp }

        EventKey (Char 'e') Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SAUp }

        EventKey (SpecialKey KeyDown) Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SADown }

        EventKey (Char 'd') Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SADown }

        EventKey (SpecialKey KeyLeft) Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SALeft }

        EventKey (Char 'a') Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SALeft }

        EventKey (SpecialKey KeyRight) Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SARight }

        EventKey (Char 'f') Down _ _ -> 
            pure $ g { _snake = updateSnakeAction _snake SARight }

        EventKey (Char 'q') Down _ _ -> 
            exitSuccess

        _ -> pure g 

    GameOver -> case e of
        EventKey (Char 'q') Down _ _ -> 
            exitSuccess

        EventKey (SpecialKey KeyEnter) Down _ _ -> 
            initGame

        _ ->
            pure g 

gameMain :: IO ()
gameMain = do
    let window = InWindow windowTitle (wWidth, wHeight) (100, 100)
    g <- initGame
    playIO window stageColor 20 g drawWorld eventHandler updateGame


