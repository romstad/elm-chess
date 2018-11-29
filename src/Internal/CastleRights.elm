module Internal.CastleRights
    exposing
        ( CastleRights
        , canCastleKingside
        , canCastleQueenside
        , disableKingsideCastling
        , disableQueensideCastling
        , doMove
        , empty
        , enableKingsideCastling
        , enableQueensideCastling
        , fromString
        , toString
        )

{- The `CastleRights` type is a simple wrapper around an `Int`. The integer is
   used as a bit field that represents the castling rights for both sides.
-}

import Bitwise exposing (..)
import Internal.Move as Move exposing (Move)
import Internal.PieceColor as PieceColor exposing (PieceColor, black, white)
import Internal.Square as Square


type CastleRights
    = CastleRights Int



{- Unwrap to Int, for shorter code. Should only be necessary in a few low level
   functions.
-}


unwrap : CastleRights -> Int
unwrap rights =
    case rights of
        CastleRights rights_ ->
            rights_



{- Empty castling rights: Neither side can castle in either direction. -}


empty : CastleRights
empty =
    CastleRights 0



{- Functions for asking whether queenside/kingside castling is allowed for the
   given side.
-}


canCastleKingside : PieceColor -> CastleRights -> Bool
canCastleKingside color rights =
    and (unwrap rights)
        (shiftLeftBy (2 * PieceColor.unwrap color) 1)
        /= 0


canCastleQueenside : PieceColor -> CastleRights -> Bool
canCastleQueenside color rights =
    and (unwrap rights)
        (shiftLeftBy (2 * PieceColor.unwrap color + 1) 1)
        /= 0



{- Updating castling rights when a move is made. -}


doMove : Move -> CastleRights -> CastleRights
doMove move rights =
    let
        from =
            Move.from move

        to =
            Move.to move
    in
        rights
            |> (if from == Square.a1 || to == Square.a1 then
                    disableQueensideCastling white
                else
                    identity
               )
            |> (if from == Square.a8 || to == Square.a8 then
                    disableQueensideCastling black
                else
                    identity
               )
            |> (if from == Square.h1 || to == Square.h1 then
                    disableKingsideCastling white
                else
                    identity
               )
            |> (if from == Square.h8 || to == Square.h8 then
                    disableKingsideCastling black
                else
                    identity
               )
            |> (if from == Square.e1 then
                    disableAllCastling white
                else
                    identity
               )
            |> (if from == Square.e8 then
                    disableAllCastling black
                else
                    identity
               )



{- Functions for adding/removing castling rights. -}


enableKingsideCastling : PieceColor -> CastleRights -> CastleRights
enableKingsideCastling color rights =
    CastleRights
        (or
            (unwrap rights)
            (shiftLeftBy (2 * PieceColor.unwrap color) 1)
        )


enableQueensideCastling : PieceColor -> CastleRights -> CastleRights
enableQueensideCastling color rights =
    CastleRights
        (or
            (unwrap rights)
            (shiftLeftBy (2 * PieceColor.unwrap color + 1) 1)
        )


disableKingsideCastling : PieceColor -> CastleRights -> CastleRights
disableKingsideCastling color rights =
    CastleRights
        (and
            (unwrap rights)
            (complement (shiftLeftBy (2 * PieceColor.unwrap color) 1))
        )


disableQueensideCastling : PieceColor -> CastleRights -> CastleRights
disableQueensideCastling color rights =
    CastleRights
        (and
            (unwrap rights)
            (complement (shiftLeftBy (2 * PieceColor.unwrap color + 1) 1))
        )


disableAllCastling : PieceColor -> CastleRights -> CastleRights
disableAllCastling color rights =
    rights
        |> disableKingsideCastling color
        |> disableQueensideCastling color



{- Translate castle rights to/from FEN strings -}


toString : CastleRights -> String
toString rights =
    let
        r =
            unwrap rights
    in
        if r == 0 then
            "-"
        else
            List.foldr
                (++)
                ""
                (List.map2
                    (\i s ->
                        if and (shiftLeftBy i 1) r /= 0 then
                            s
                        else
                            ""
                    )
                    (List.range 0 3)
                    [ "K", "Q", "k", "q" ]
                )


fromChar : Char -> Int
fromChar char =
    if char == 'K' then
        1
    else if char == 'Q' then
        2
    else if char == 'k' then
        4
    else if char == 'q' then
        8
    else
        0


fromString : String -> CastleRights
fromString string =
    CastleRights
        (List.foldl
            or
            0
            (List.map fromChar (String.toList string))
        )
