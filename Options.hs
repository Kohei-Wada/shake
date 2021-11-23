module Options where

import Graphics.Gloss.Interface.IO.Game

nFoods :: Int
nFoods = 5


stageColor :: Color
stageColor = light $ light $ light $ green


snakeHeadColor :: Color
snakeHeadColor = blue


snakeTailColor :: Color
snakeTailColor = light $ light $ blue


foodColor :: Color
foodColor = red


wWidth, wHeight :: Num a => a
wWidth  = 1000
wHeight = 1000


windowTitle :: String
windowTitle = "Shake Game"


cellSize, cellWidth, cellHeight :: Num a => a
cellSize   = 20
cellWidth  = fromIntegral $ wWidth  `div` cellSize
cellHeight = fromIntegral $ wHeight `div` cellSize
