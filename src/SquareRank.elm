module SquareRank exposing
    ( SquareRank
    , fromChar, fromString, toChar, toString
    , all, one, two, three, four, five, six, seven, eight
    , distance, toIndex
    )

{-| The `SquareRank` data type and related functions and definitions.


# Types

@docs SquareRank


# Converting to and from Strings and Characters

@docs fromChar, fromString, toChar, toString


# Miscellaneous Useful Constants

@docs all, one, two, three, four, five, six, seven, eight


# Miscellaneous Functions

@docs distance, toIndex

-}

import Internal.SquareRank as Internal


{-| A `SquareRank` is a type representing a rank on a chess board, i.e. one of
the horizontal rows labeled 1-8.
-}
type alias SquareRank =
    Internal.SquareRank


{-| Converts a `SquareRank` to a `Char` in the range 1-8.

    toChar three == '3'

    List.map toChar all == [ '1', '2', '3', '4', '5', '6', '7', '8' ]

-}
toChar : SquareRank -> Char
toChar =
    Internal.toChar


{-| Converts a `SquareRank` to a single-character `String` consisting of a
letter in the range 1-8.

    toString three == "3"

    List.map toString all == [ "1", "2", "3", "4", "5", "6", "7", "8" ]

-}
toString : SquareRank -> String
toString =
    Internal.toString


{-| Tries to convert a `Char` to a `SquareRank`. Returns `Nothing` if the
character is not a digit in the range 1-8.

    fromChar '5' == Just five

    List.map fromChar [ '1', 'x', '8' ] == [ Just one, Nothing, Just eight ]

-}
fromChar : Char -> Maybe SquareRank
fromChar =
    Internal.fromChar


{-| Tries to convert a `String` to a `SquareRank` by looking at the first
character of the string. Returns `Nothing` if the first character is not a
digit in the range 1-8.

    fromChar '5' == Just five

    List.map fromChar [ '1', 'x', '8' ] == [ Just one, Nothing, Just eight ]

-}
fromString : String -> Maybe SquareRank
fromString =
    Internal.fromString


{-| List of all ranks on the board.
-}
all : List SquareRank
all =
    Internal.all


{-| The first rank, seen from white's point of view.
-}
one : SquareRank
one =
    Internal.one


{-| The second rank, seen from white's point of view.
-}
two : SquareRank
two =
    Internal.two


{-| The third rank, seen from white's point of view.
-}
three : SquareRank
three =
    Internal.three


{-| The fourth rank, seen from white's point of view.
-}
four : SquareRank
four =
    Internal.four


{-| The fifth rank, seen from white's point of view.
-}
five : SquareRank
five =
    Internal.five


{-| The sixth rank, seen from white's point of view.
-}
six : SquareRank
six =
    Internal.six


{-| The seventh rank, seen from white's point of view.
-}
seven : SquareRank
seven =
    Internal.seven


{-| The eighth rank, seen from white's point of view.
-}
eight : SquareRank
eight =
    Internal.eight


{-| The vertical distance between two ranks.

    distance one eight == 7

    distance five three == 2

-}
distance : SquareRank -> SquareRank -> Int
distance =
    Internal.distance


{-| Convert a rank to an index in the range 0 (for the first rank) to 7 (for the
last rank).
-}
toIndex : SquareRank -> Int
toIndex =
    Internal.toIndex
