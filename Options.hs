module Options where

wWidth, wHeight :: Num a => a
wWidth  = 600
wHeight = 600


windowTitle :: String
windowTitle = "Snake Game"


cellSize, cellWidth, cellHeight :: Num a => a
cellSize   = 20
cellWidth  = fromIntegral $ wWidth  `div` cellSize
cellHeight = fromIntegral $ wHeight `div` cellSize



