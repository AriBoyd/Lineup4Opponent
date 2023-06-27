data Player = Red | Yellow

type Piece = Maybe Player

type Column = (Piece, Piece, Piece, Piece, Piece, Piece)

type Board = (Column, Column, Column, Column, Column, Column, Column)

data Move = Zero | One | Two | Three | Four | Five | Six deriving (Read, Show, Enum, Eq, Ord)

type GameState = (Board, Player)

nextState :: GameState -> Move -> GameState
nextState state move = placePiece (board !! move) player
  where
    (board, player) = state