module Position exposing
    ( Position
    , initial
    , sideToMove, pieceOn, colorOn, isEmpty, epSquare, moveNumber, sideAttacksSquare, isCheck, isCheckmate
    , moves, movesFrom, doMove
    , fromFen, toFen, toUci
    )

{-| This module defines the `Position` data type, which is used to model
chess positions.


# Types

@docs Position


# Useful Constants

@docs initial


# Properties of Positions

@docs sideToMove, pieceOn, colorOn, isEmpty, epSquare, moveNumber, sideAttacksSquare, isCheck, isCheckmate


# Generating and Making Moves

@docs moves, movesFrom, doMove


# Converting to and from UCI/FEN Strings

@docs fromFen, toFen, toUci

-}

import Internal.Piece
import Internal.Position as Internal
import Move exposing (Move)
import Piece exposing (Piece)
import PieceColor exposing (PieceColor)
import Square exposing (Square)


{-| Type representing a chess position.
-}
type alias Position =
    Internal.Position


{-| The standard starting position.
-}
initial : Position
initial =
    Internal.initial


{-| The current side to move.
-}
sideToMove : Position -> PieceColor
sideToMove =
    Internal.sideToMove


{-| The piece on the given square, or `Nothing` if the square is empty.
-}
pieceOn : Square -> Position -> Maybe Piece
pieceOn square pos =
    let
        p =
            Internal.pieceOn square pos
    in
    if p == Internal.Piece.empty || p == Internal.Piece.outside then
        Nothing

    else
        Just p


{-| The color of the piece on the given square, or `Nothing` if the square is
empty.
-}
colorOn : Square -> Position -> Maybe PieceColor
colorOn square pos =
    Maybe.map Piece.color <| pieceOn square pos


{-| Tests whether the given square on the board is empty.
-}
isEmpty : Square -> Position -> Bool
isEmpty =
    Internal.isEmpty


{-| The en passant square, or `Nothing` if no en passant capture is possible.
-}
epSquare : Position -> Maybe Square
epSquare =
    Internal.epSquare


{-| Current move number in the game.
-}
moveNumber : Position -> Int
moveNumber =
    Internal.moveNumber


{-| Tests whether the given side attacks the given square.
-}
sideAttacksSquare : PieceColor -> Square -> Position -> Bool
sideAttacksSquare =
    Internal.sideAttacksSquare


{-| Tests whether the side to move is in check.
-}
isCheck : Position -> Bool
isCheck =
    Internal.isCheck


{-| Tests whether the side to move is checkmated.
-}
isCheckmate : Position -> Bool
isCheckmate =
    Internal.isCheckmate


{-| Do a move on the board, returning the resulting position.
-}
doMove : Move -> Position -> Position
doMove =
    Internal.doMove


{-| A list of all legal moves.
-}
moves : Position -> List Move
moves =
    Internal.moves


{-| A list of all legal moves from the given square.
-}
movesFrom : Square -> Position -> List Move
movesFrom =
    Internal.movesFrom


{-| Tries to initialize a position from a FEN string. Returns `Nothing` on
failure.
-}
fromFen : String -> Maybe Position
fromFen =
    Internal.fromFen


{-| Converts a position to a `String` in FEN notation.
-}
toFen : Position -> String
toFen =
    Internal.toFen


{-| Converts a position to UCI notation, for sending it to a chess engine with
a `position fen <fen> moves ...` command.
-}
toUci : Position -> String
toUci =
    Internal.toUci
