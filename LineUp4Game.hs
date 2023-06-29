import Data.List (findIndices)

-- There are two players, Red and Yellow
data Player = Red | Yellow

-- A space can have either player's token, or be empty
data Space = Player | Empty

type Column = [Space] -- 0th index is considered the top row, 6th is the bottom

type Board = [Column]

type Move = Int

-- The game is entirely defined by the board and whose turn it is
type GameState = (Board, Player)

swapPlayer :: Player -> Player
swapPlayer Red = Yellow
swapPlayer Yellow = Red

placeTokenColumn :: Column -> Player -> Column
placeTokenColumn [] _ = error "Empty Column"
placeTokenColumn (Empty : Empty : rest) player = Empty : placeTokenColumn (Empty : rest) player -- Descend a row
placeTokenColumn (Empty : rest) player = player : rest -- Found the lowest empty space
placeTokenColumn _ _ = error "No Free Space"

placeToken :: Board -> Move -> Player -> Board
placeToken board move player = take move board ++ [placeTokenColumn (board !! move) player] ++ drop (move + 1) board

nextState :: GameState -> Move -> GameState
nextState state move = (placeToken board move player, swapPlayer player)
  where
    (board, player) = state

freeSpace :: Column -> Bool
freeSpace (Empty : _) = True -- Since pieces fall, any non-full column has an empty space at the top
freeSpace _ = False

getValidMoves :: GameState -> [Move]
getValidMoves (board, _) = findIndices freeSpace board

compareWins :: Maybe Player -> Maybe Player -> Maybe Player
compareWins Nothing Nothing = Nothing
compareWins player Nothing = player
compareWins Nothing player = player -- There should only ever be one winner
compareWins player1 player2
    | player1==player2 = player1
    | otherwise = error "Both players somehow won"

checkWinnerLine :: [Space] -> Maybe Player -- Test if a list of spaces has a four in a line
checkWinnerLine (x1 : x2 : x3 : x4 : restLine)
  | x1 == x2 == x3 == x4 = Just x1 -- We found a four in a line
  | otherwise = checkWinnerLine (x2 : x3 : x4 : restCol) -- Check the rest of the line
checkWinnerLine _ = Nothing -- There are no vertical four in a lines

checkWinnerRow :: [Column] -> Maybe Player -- Check the top row of the columns
checkWinnerRow ()

checkWinnerRows :: Board -> Maybe Player --Check each row

checkWinnerCols :: Board -> Maybe Player
checkWinnerCols [] = Nothing -- There are no vertical four in a lines
checkWinnerCols ([_,_,_]:rest) = checkWinnerCols rest -- Can't have four in a line with three elements

checkWinnerDiagLRs :: Board -> Maybe Player

checkWinnerDiagRLs :: Board -> Maybe Player

checkWinner :: GameState -> Maybe Player -- Assumes there will be at most one winner, since the game ends after a player wins
checkWinner (board,_) = 