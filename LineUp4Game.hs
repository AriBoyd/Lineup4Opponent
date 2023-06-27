data Piece = Yellow | Red | Empty

type Row = (Piece, Piece, Piece, Piece, Piece, Piece, Piece)

type Board = (Row, Row, Row, Row, Row, Row)

data Turn = Player1 | Player2

type GameState = (Board, Turn)