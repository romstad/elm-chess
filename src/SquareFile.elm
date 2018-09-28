module SquareFile exposing
    ( SquareFile
    , fromChar, fromString, toChar, toString
    , all, a, b, c, d, e, f, g, h
    , distance, toIndex
    )

{-| The `SquareFile` data type and related functions and definitions.


# Types

@docs SquareFile


# Converting to and from Strings and Characters

@docs fromChar, fromString, toChar, toString


# Miscellaneous Useful Constants

@docs all, a, b, c, d, e, f, g, h


# Miscellaneous Functions

@docs distance, toIndex

-}

import Internal.SquareFile as Internal


{-| `SquareFile` is a type representing a file on a chess board, i.e. one of
the vertical columns labeled a-h.
-}
type alias SquareFile =
    Internal.SquareFile


{-| Converts a `SquareFile` to a `Char` in the range a-h.

    toChar c == 'c'

    List.map toChar all == [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' ]

-}
toChar : SquareFile -> Char
toChar =
    Internal.toChar


{-| Converts a `SquareFile` to a single-character `String` consisting of a
letter in the range a-h.

    toString c == "c"

    List.map toString all == [ "a", "b", "c", "d", "e", "f", "g", "h" ]

-}
toString : SquareFile -> String
toString =
    Internal.toString


{-| Tries to convert a `Char` to a `SquareFile`. Returns `Nothing` if the
character is not a lowercase letter in the range a-h.

    fromChar 'e' == Just e

    List.map fromChar [ 'a', 'x', 'h' ] == [ Just a, Nothing, Just h ]

-}
fromChar : Char -> Maybe SquareFile
fromChar =
    Internal.fromChar


{-| Tries to convert a `String` to a `SquareFile` by looking at the first
character of the string. Returns `Nothing` if the first character is not a
lowercase letter in the range a-h.

    fromString "e" == Just e

    List.map fromChar [ "a", "x", "h" ] == [ Just a, Nothing, Just h ]

-}
fromString : String -> Maybe SquareFile
fromString =
    Internal.fromString


{-| List of all files on the board.
-}
all : List SquareFile
all =
    Internal.all


{-| The 'a' file, the first file from the left, from white's point of view.
-}
a : SquareFile
a =
    Internal.a


{-| The 'b' file, the second file from the left, from white's point of view.
-}
b : SquareFile
b =
    Internal.b


{-| The 'c' file, the third file from the left, from white's point of view.
-}
c : SquareFile
c =
    Internal.c


{-| The 'd' file, the fourth file from the left, from white's point of view.
-}
d : SquareFile
d =
    Internal.d


{-| The 'e' file, the fifth file from the left, from white's point of view.
-}
e : SquareFile
e =
    Internal.e


{-| The 'f' file, the sixth file from the left, from white's point of view.
-}
f : SquareFile
f =
    Internal.f


{-| The 'g' file, the seventh file from the left, from white's point of view.
-}
g : SquareFile
g =
    Internal.g


{-| The 'h' file, the eighth file from the left, from white's point of view.
-}
h : SquareFile
h =
    Internal.h


{-| The horizontal distance between two files.

    distance a h == 7

    distance e c == 2

-}
distance : SquareFile -> SquareFile -> Int
distance =
    Internal.distance


{-| Convert a file to an index in the range 0 (for the 'a' file) to 7 (for the
'h' file).
-}
toIndex : SquareFile -> Int
toIndex =
    Internal.toIndex
