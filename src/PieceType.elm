module PieceType exposing
    ( PieceType
    , pawn, knight, bishop, rook, queen, king, all, promotionPieces
    , fromChar, fromString, toChar, toString
    )

{-| This module defines the `PieceType` type and related functions. A
`PieceType` value is basically an uncolored chess piece.


# Types

@docs PieceType


# Useful Constants

@docs pawn, knight, bishop, rook, queen, king, all, promotionPieces


# Conversion to and from Strings and Chars

@docs fromChar, fromString, toChar, toString

-}

import Internal.PieceType as Internal


{-| A type representing the type of a chess piece, i.e. pawn, knight, bishop,
etc. It's essentially a chess piece without information about its color.
-}
type alias PieceType =
    Internal.PieceType


{-| Value representing a pawn.
-}
pawn : PieceType
pawn =
    Internal.pawn


{-| Value representing a knight.
-}
knight : PieceType
knight =
    Internal.knight


{-| Value representing a bishop.
-}
bishop : PieceType
bishop =
    Internal.bishop


{-| Value representing a rook.
-}
rook : PieceType
rook =
    Internal.rook


{-| Value representing a queen.
-}
queen : PieceType
queen =
    Internal.queen


{-| Value representing a king.
-}
king : PieceType
king =
    Internal.king


{-| List of all piece types.

    all =
        [ pawn, knight, bishop, rook, queen, king ]

-}
all : List PieceType
all =
    Internal.all


{-| List of all piece types to which a pawn can promote.

    promotionPieces =
        [ queen, rook, bishop, knight ]

-}
promotionPieces : List PieceType
promotionPieces =
    Internal.promotionPieces


{-| Converts a `PieceType` to the corresponding character used in English
algebraic notation.

    toChar pawn == 'P'

    toChar knight == 'N'

    toChar bishop == 'B'

    toChar rook == 'R'

    toChar queen == 'Q'

    toChar king == 'K'

-}
toChar : PieceType -> Char
toChar =
    Internal.toChar


{-| Converts a `PieceType` to the a single-character string containing the
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
    Internal.toString


{-| Tries to convert a character to a `PieceType`. Accepts both uppercase
and lowercase letters; returns `Nothing` if the character is not one of the
piece letters used in English algebraic notation.

    fromChar 'P' == Just pawn

    fromChar 'r' == Just rook

    fromChar 'x' == Nothing

-}
fromChar : Char -> Maybe PieceType
fromChar =
    Internal.fromChar


{-| Tries to convert a string to a `PieceType`, based on the first character
in the string. Accepts both uppercase and lowercase letters; returns
`Nothing` if the character is not one of the piece letters used in English
algebraic notation.

    fromString "k" == Just king

    fromString "Qa4+" == Just queen

    fromString "x" == Nothing

-}
fromString : String -> Maybe PieceType
fromString =
    Internal.fromString
