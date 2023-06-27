data Player = Red | Yellow

type Piece = Maybe Player

type Row = (Piece, Piece, Piece, Piece, Piece, Piece, Piece)

type Board = (Row, Row, Row, Row, Row, Row)

type GameState = (Board, Player)
