module Internal.Board
    exposing
        ( Board
        , doMove
        , empty
        , fromFen
        , isEmpty
        , lineIsClear
        , movePiece
        , pieceAttacksSquare
        , pieceOn
        , putPiece
        , removePiece
        , scan
        , sideAttacksSquare
        , toFen
        )

import Array exposing (Array)
import Char
import Internal.BoardDimensions exposing (..)
import Internal.Move as Move exposing (Move)
import Internal.Piece as Piece exposing (Piece(..))
import Internal.PieceColor as PieceColor exposing (PieceColor)
import Internal.Square as Square exposing (Square(..))
import Internal.SquareDelta as Delta exposing (SquareDelta)


type alias Board =
    Array Piece



{- An empty board, initialized to `Piece.empty` values on all squares of the
   "real" board, and `Piece.outside` values everywhere else.
-}


empty : Board
empty =
    List.foldl
        (\s b ->
            Array.set (Square.unwrap s) Piece.empty b
        )
        (Array.fromList (List.repeat extendedBoardSize Piece.outside))
        Square.all



{- Get the piece on the given square. In the case of an out of range square
   (which shouldn't be possible), we return `Piece.outside`.
-}


pieceOn : Square -> Board -> Piece
pieceOn square board =
    Maybe.withDefault Piece.outside (Array.get (Square.unwrap square) board)



{- Tests whether the given square on the board is empty. -}


isEmpty : Square -> Board -> Bool
isEmpty square board =
    pieceOn square board == Piece.empty



{- Do a move on the board, returing the new board state after the move is
   executed.
-}


doMove : Move -> Board -> Board
doMove move board =
    let
        us =
            Piece.color (pieceOn (Move.from move) board)

        from =
            Move.from move

        to =
            Move.to move
    in
        case Move.promotion move of
            Nothing ->
                if Move.isKingsideCastle move then
                    board
                        |> movePiece from to
                        |> movePiece
                            (Square.add to Delta.e)
                            (Square.add from Delta.e)
                else if Move.isQueensideCastle move then
                    board
                        |> movePiece from to
                        |> movePiece
                            (Square.add to (Delta.multiply 2 Delta.w))
                            (Square.add from Delta.w)
                else if Move.isEp move then
                    let
                        fromRank =
                            Square.rank from

                        toFile =
                            Square.file to
                    in
                        board
                            |> movePiece from to
                            |> removePiece (Square.make toFile fromRank)
                else
                    movePiece from to board

            Just kind ->
                board
                    |> removePiece from
                    |> putPiece (Piece.make us kind) to



{- Put the given piece on the given square of the board, and return the modified
   board.
-}


putPiece : Piece -> Square -> Board -> Board
putPiece piece square =
    Array.set (Square.unwrap square) piece



{- Remove the piece on the given square of the board, and return the modified
   board.
-}


removePiece : Square -> Board -> Board
removePiece square =
    Array.set (Square.unwrap square) Piece.empty



{- Move a piece from one square to another, and return the modified board. If
   the destination square is not empty, the piece on that square will vanish
   without a trace.
-}


movePiece : Square -> Square -> Board -> Board
movePiece from to board =
    let
        piece =
            pieceOn from board
    in
        board
            |> removePiece from
            |> putPiece piece to



{- Initialize a board from a FEN string. -}


fromFen : String -> Board
fromFen fen =
    .board
        (List.foldl
            processFenChar
            (ReadFenState empty 0 (rankCount - 1))
            (String.toList fen)
        )



{- Export a board to a FEN string. -}


toFen : Board -> String
toFen board =
    .string
        (List.foldl
            applyTransition
            { string = "", skip = 0 }
            (boardToTransitions board)
        )



{- Scan the board from a given start square along a given direction (a square
   delta, assumed to be one of the eight sliding direction) until a non-empty
   or outside square is reached. Returns the first non-empty or outside square
   found in this direction.
-}


scan : Board -> Square -> SquareDelta -> Square
scan board square delta =
    let
        scanInternal s =
            if pieceOn s board /= Piece.empty then
                s
            else
                scanInternal (Square.add s delta)
    in
        scanInternal (Square.add square delta)



{- Checks that the ray between two colinear squares is clear from obstructions
   along the given direction.
-}


lineIsClear : Board -> Square -> Square -> SquareDelta -> Bool
lineIsClear board square0 square1 delta =
    let
        lineIsClearInternal s0 s1 =
            (s0 == square1)
                || ((pieceOn s0 board == Piece.empty)
                        && lineIsClearInternal (Square.add s0 delta) s1
                   )
    in
        lineIsClearInternal (Square.add square0 delta) square1



{- Checks whether the piece on square `from` attacks square `to` -}


pieceAttacksSquare : Square -> Square -> Board -> Bool
pieceAttacksSquare from to board =
    let
        piece =
            pieceOn from board
    in
        case Piece.attackDelta piece from to of
            Nothing ->
                False

            Just delta ->
                if Piece.isSlider piece then
                    lineIsClear board from to delta
                else
                    True



{- Checks whether the given side attacks the given square. -}


sideAttacksSquare : PieceColor -> Square -> Board -> Bool
sideAttacksSquare side square board =
    List.any
        (\s ->
            (Piece.color (pieceOn s board) == side)
                && pieceAttacksSquare s square board
        )
        Square.all



{- Helper functions for parsing FEN boards. -}


type alias ReadFenState =
    { board : Board
    , fileIndex : Int
    , rankIndex : Int
    }


readFenPiece : Piece -> ReadFenState -> Board
readFenPiece piece state =
    putPiece
        piece
        (Square.expand (state.fileIndex + 8 * state.rankIndex))
        state.board


processFenChar : Char -> ReadFenState -> ReadFenState
processFenChar char state =
    case Piece.fromChar char of
        Just piece ->
            { state
                | board = readFenPiece piece state
                , fileIndex = state.fileIndex + 1
            }

        Nothing ->
            if Char.isDigit char then
                { state
                    | fileIndex =
                        state.fileIndex
                            + Char.toCode char
                            - Char.toCode '0'
                }
            else if char == '/' then
                { state
                    | fileIndex = 0
                    , rankIndex = state.rankIndex - 1
                }
            else
                state



{- Helper functions for exporting a board to a FEN string. -}


type alias WriteFenState =
    { string : String
    , skip : Int
    }


type WriteFenStateTransition
    = AddPiece Piece
    | NextRank
    | Done


writeFenPiece : Piece -> WriteFenState -> WriteFenState
writeFenPiece piece state =
    if piece == Piece.empty then
        { state | skip = state.skip + 1 }
    else if state.skip > 0 then
        { string =
            state.string
                ++ String.fromInt state.skip
                ++ Piece.toString piece
        , skip = 0
        }
    else
        { string = state.string ++ Piece.toString piece
        , skip = 0
        }


fenNextRank : WriteFenState -> WriteFenState
fenNextRank state =
    if state.skip > 0 then
        { string = state.string ++ String.fromInt state.skip ++ "/"
        , skip = 0
        }
    else
        { string = state.string ++ "/"
        , skip = 0
        }


fenDone : WriteFenState -> WriteFenState
fenDone state =
    if state.skip > 0 then
        { string = state.string ++ String.fromInt state.skip
        , skip = 0
        }
    else
        state


applyTransition : WriteFenStateTransition -> WriteFenState -> WriteFenState
applyTransition transition =
    case transition of
        AddPiece piece ->
            writeFenPiece piece

        NextRank ->
            fenNextRank

        Done ->
            fenDone


squaresForRankIndex : Int -> Board -> List Square
squaresForRankIndex rankIndex board =
    List.map
        (\f -> Square.expand (f + rankIndex * 8))
        (List.range 0 (fileCount - 1))


rankToTransitions : Int -> Board -> List WriteFenStateTransition
rankToTransitions rankIndex board =
    List.map
        (\s -> AddPiece (pieceOn s board))
        (squaresForRankIndex rankIndex board)


boardToTransitions : Board -> List WriteFenStateTransition
boardToTransitions board =
    List.concat
        (List.intersperse
            [ NextRank ]
            (List.map
                (\r -> rankToTransitions r board)
                (List.reverse (List.range 0 (rankCount - 1)))
            )
        )
        ++ [ Done ]
