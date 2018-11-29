module PieceColor exposing
    ( PieceColor
    , white, black, all
    , opposite
    , fromChar, fromString, toChar, toString
    )

{-| This module defines the PieceColor type and related functions.


# Types

@docs PieceColor


# Useful Constants

@docs white, black, all


# Inverting a Color

@docs opposite


# Converting to Strings and Characters

@docs fromChar, fromString, toChar, toString

-}

import Internal.PieceColor as Internal


{-| Type representing the color of a chess piece, or one of the two players.
-}
type alias PieceColor =
    Internal.PieceColor


{-| The color of a white piece.
-}
white : PieceColor
white =
    Internal.white


{-| The color of a black piece.
-}
black : PieceColor
black =
    Internal.black


{-| List of all piece colors.

    all == [ white, black ]

-}
all : List PieceColor
all =
    Internal.all


{-| The opposite of a color.

    opposite white == black

    opposite black == white

-}
opposite : PieceColor -> PieceColor
opposite =
    Internal.opposite


{-| Convert a `PieceColor` to a `Char` of the form used when representing a
board in Forsyth-Edwards notation.

    toChar white == 'w'

    toChar black == 'b'

-}
toChar : PieceColor -> Char
toChar =
    Internal.toChar


{-| Convert a `PieceColor` to a single-character `String` containing a
`Char` of the form used when representing a board in Forsyth-Edwards notation.

    toChar white == "w"

    toChar black == "b"

-}
toString : PieceColor -> String
toString =
    Internal.toString


{-| Tries to convert a character to a `PieceColor`, using Forsyth-Edwards
encoding.
fromChar 'w' == Just white
fromChar 'b' == Just black
fromChar ch == Nothing -- for all ch not equal to 'w' or 'b'
-}
fromChar : Char -> Maybe PieceColor
fromChar =
    Internal.fromChar


{-| Tries to convert a string to a `PieceColor`, using Forsyth-Edwards
encoding.
fromString "w" == Just white
fromString "b" == Just black
fromString str == Nothing -- for all str not starting with "w" or "b"
-}
fromString : String -> Maybe PieceColor
fromString =
    Internal.fromString
