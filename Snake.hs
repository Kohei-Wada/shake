module Snake where


type Position = (Int, Int)
type Snake = [Position]

data SnakeAction = SAStop | SAUp | SADown | SALeft | SARight deriving Eq

moveSnake :: SnakeAction -> Position -> Position
moveSnake SAStop  (x, y) = (x, y)
moveSnake SAUp    (x, y) = (x, y + 1)
moveSnake SADown  (x, y) = (x, y - 1)
moveSnake SALeft  (x, y) = (x - 1, y)
moveSnake SARight (x, y) = (x + 1, y)


