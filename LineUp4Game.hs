import Data.List (findIndices)
import Data.Maybe (isNothing)

-- There are two players, Red and Yellow
data Player = Red | Yellow deriving (Eq)

instance Show Player where
  show Red = "X"
  show Yellow = "O"

-- A space can have either player's token, or be empty
data Space = Colour Player | Empty deriving (Eq)

instance Show Space where
  show Colour player = show player
  show Empty = " "

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
placeTokenColumn (Empty : rest) player = Colour player : rest -- Found the lowest empty space
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

-- Allows folding results of the different line up 4 checks
compareWins :: Maybe Player -> Maybe Player -> Maybe Player
compareWins Nothing Nothing = Nothing
compareWins player Nothing = player
compareWins Nothing player = player -- There should only ever be one winner
compareWins player1 player2
  | player1 == player2 = player1
  | otherwise = error "Both players somehow won"

checkWinnerLine :: [Space] -> Maybe Player -- Test if a list of spaces has a four in a line
checkWinnerLine line
  | length line < 4 = Nothing -- There are less than four spaces left
  | head line == Empty = checkWinnerLine (tail line) -- Check the rest of the line
  | all (== Colour Red) (take 4 line) = Just Red
  | all (== Colour Yellow) (take 4 line) = Just Yellow
  | otherwise = checkWinnerLine (tail line) -- Check the rest of the line

checkWinnerRows :: Board -> Maybe Player -- Check each row
checkWinnerRows [] = Nothing
checkWinnerRows rows
  | isNothing firstRowWinner = checkWinnerRows (map tail rows)
  | otherwise = firstRowWinner
  where
    firstRowWinner = checkWinnerLine (map head rows)

checkWinnerCols :: Board -> Maybe Player
checkWinnerCols [] = Nothing
checkWinnerCols cols
  | isNothing firstColWinner = checkWinnerCols (tail cols)
  | otherwise = firstColWinner
  where
    firstColWinner = checkWinnerLine (head cols)

checkWinnerDiags :: Board -> Maybe Player
checkWinnerDiags [] = Nothing
checkWinnerDiags board
  | length board < 4 || length (head board) < 4 = Nothing
  | isNothing firstDiagWinner = compareWins (checkWinnerDiags (tail board)) (checkWinnerDiags (map tail board))
  | otherwise = firstDiagWinner
  where
    firstDiagLRWinner = checkWinnerLine [board !! x !! x | x <- [0 .. 3]] -- Get the left right diagonal
    firstDiagRLWinner = checkWinnerLine [board !! x !! y | x <- [0 .. 3], y <- [3, 2 .. 0]] -- Get the left right diagonal
    firstDiagWinner = compareWins firstDiagLRWinner firstDiagRLWinner

checkWinner :: GameState -> Maybe Player -- Assumes there will be at most one winner, since the game ends after a player wins
checkWinner (board, _) = foldl1 compareWins (map ($ board) [checkWinnerRows, checkWinnerCols, checkWinnerDiags])

initialiseGame :: GameState
initialiseGame = ([[Empty | _ <- [1 .. 6]] | _ <- [1 .. 7]], Red) -- An empty 7x6 board, and Red starts

displayState :: GameState -> String -- Displays a gamestate, without trying to override the Show method
displayState ([], player) = "It is " ++ show player ++ "'s turn"
displayState (board, player) = show (map head board) ++ "\n" ++ displayState (show (map tail board))