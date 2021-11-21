module Options where


nFoods :: Int
nFoods = 5

wWidth, wHeight :: Num a => a
wWidth  = 1000
wHeight = 1000

windowTitle :: String
windowTitle = "Shake Game"

cellSize, cellWidth, cellHeight :: Num a => a
cellSize   = 20
cellWidth  = fromIntegral $ wWidth  `div` cellSize
cellHeight = fromIntegral $ wHeight `div` cellSize

