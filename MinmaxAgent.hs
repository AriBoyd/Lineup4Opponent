module MinmaxAgent where

valueFunc :: GameState -> int
valueFunc state
    | winner == Nothing = 0
    | winner = player = 1
    | otherwise = -1
    where winner = checkWinner state
    (player, board = state)

takeTurnMinmax :: GameState -> IO Move
takeTurnMinmax =