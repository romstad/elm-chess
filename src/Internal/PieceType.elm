module Internal.PieceType
    exposing
        ( PieceType(..)
        , none
        , pawn
        , knight
        , bishop
        , rook
        , queen
        , king
        , all
        , promotionPieces
        , isSlider
        , fromChar
        , fromString
        , toChar
        , toString
        , unwrap
        )

{-| This module defines the PieceType type and related functions. A
PieceType value is basically an uncolored chess piece.


# Types

@docs PieceType


# Common Constants

@docs none, pawn, knight, bishop, rook, queen, king, all, promotionPieces


# Properties of Pieces

@docs isSlider


# Conversion to and from Strings and Characters

@docs fromChar, fromString, toChar, toString

-}

import Char


type PieceType
    = PieceType Int


{-| Unwrap a PieceType to an Int. Should only be necessary in a few low level
functions. You almost should almost certainly not use this in any external
code.

    unwrap pawn == 1
    unwrap knight == 2
    unwrap bishop == 3
    unwrap rook = 4
    unwrap queen = 5
    unwrap king = 6

-}
unwrap : PieceType -> Int
unwrap kind =
    case kind of
        PieceType kind_ ->
            kind_


{-| The special value "none" is the type of the "piece" that occupies
an empty square, or a square outside the board.
-}
none : PieceType
none =
    PieceType 0


{-| Value representing a pawn.
-}
pawn : PieceType
pawn =
    PieceType 1


{-| Value representing a knight.
-}
knight : PieceType
knight =
    PieceType 2


{-| Value representing a bishop.
-}
bishop : PieceType
bishop =
    PieceType 3


{-| Value representing a rook.
-}
rook : PieceType
rook =
    PieceType 4


{-| Value representing a queen.
-}
queen : PieceType
queen =
    PieceType 5


{-| Value representing a king.
-}
king : PieceType
king =
    PieceType 6


{-| List of all "real" piece types.

    all =
        [ pawn, knight, bishop, rook, queen, king ]

-}
all : List PieceType
all =
    [ pawn, knight, bishop, rook, queen, king ]


{-| The piece types to which a pawn can promote when reaching the last rank.

    promotionPieces == [ queen, knight, rook, bishop ]

-}
promotionPieces : List PieceType
promotionPieces =
    [ queen, knight, rook, bishop ]


{-| Tests whether a PieceType is a sliding piece, i.e. whether it can move
multiple squares in the same direction as long as the path is unobstructed.

    isSlider knight == False
    isSlider rook = True

-}
isSlider : PieceType -> Bool
isSlider kind =
    kind == bishop || kind == rook || kind == queen


{-| Converts a PieceType to the corresponding character used in English
algebraic notation.

    toChar pawn == 'P'

    toChar knight == 'N'

    toChar bishop == 'B'

    toChar rook == 'R'

    toChar queen == 'Q'

    toChar king == 'K'

-}
toChar : PieceType -> Char
toChar kind =
    if kind == pawn then
        'P'
    else if kind == knight then
        'N'
    else if kind == bishop then
        'B'
    else if kind == rook then
        'R'
    else if kind == queen then
        'Q'
    else if kind == king then
        'K'
    else
        '?'


{-| Converts a PieceType to the a single-character string containing the
corresponding piece letter used in English algebraic notation.

    toString pawn == "P"

    toString knight == "N"

    toString bishop == "B"

    toString rook == "R"

    toString queen == "Q"

    toString king == "K"

-}
toString : PieceType -> String
toString =
    toChar >> String.fromChar


{-| Tries to convert a character to a PieceType. Accepts both uppercase
and lowercase letters; returns Nothing if the character is not one of the
piece letters used in English algebraic notation.

    fromChar 'P' == Just pawn

    fromChar 'r' == Just rook

    fromChar 'x' == Nothing

-}
fromChar : Char -> Maybe PieceType
fromChar char =
    let
        ch =
            Char.toUpper char
    in
        if ch == 'P' then
            Just pawn
        else if ch == 'N' then
            Just knight
        else if ch == 'B' then
            Just bishop
        else if ch == 'R' then
            Just rook
        else if ch == 'Q' then
            Just queen
        else if ch == 'K' then
            Just king
        else
            Nothing


{-| Tries to convert a string to a PieceType, based on the first character
in the string. Accepts both uppercase and lowercase letters; returns
Nothing if the character is not one of the piece letters used in English
algebraic notation.

    fromString "k" == Just king

    fromString "Qa4+" == Just queen

    fromString "x" == Nothing

-}
fromString : String -> Maybe PieceType
fromString string =
    Maybe.andThen fromChar (List.head (String.toList string))
