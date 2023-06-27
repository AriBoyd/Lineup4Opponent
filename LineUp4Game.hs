-- There are two players, Red and Yellow
data Player = Red | Yellow

-- A spot can have either player's token, or be empty
type Spot = Maybe Player

type Column = [Spot]

type Board = [Column]

type Move = Int

-- The game is entirely defined by the board and whose turn it is
type GameState = (Board, Player)

swapPlayer :: Player -> Player
swapPlayer Red = Yellow
swapPlayer Yellow = Red

placeTokenColumn :: Column -> Player -> Column
placeTokenColumn [] _ = error "Empty Column"
placeTokenColumn (Nothing : Nothing : rest) player = Nothing : placeTokenColumn (Nothing : rest) player -- Descend a row
placeTokenColumn (Nothing : rest) player = Just player : rest -- Found the spot
placeTokenColumn _ _ = error "No Free Space"

nextState :: GameState -> Move -> GameState
nextState state move = placeToken board move player swapPlayer player
  where
    (board, player) = state