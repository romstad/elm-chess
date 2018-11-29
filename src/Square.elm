module Square exposing
    ( Square
    , make, file, rank
    , fromString, toString, fromInt, toInt
    , fileDistance, rankDistance, distance
    , all, a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, b3, c3, d3, e3, f3, g3, h3, a4, b4, c4, d4, e4, f4, g4, h4, a5, b5, c5, d5, e5, f5, g5, h5, a6, b6, c6, d6, e6, f6, g6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8
    )

{-| The `Square` data type and related functions and definitions.


# Types

@docs Square


# Manipulating Squares

@docs make, file, rank


# Converting to and from Strings and Ints

@docs fromString, toString, fromInt, toInt


# Distances between Squares

@docs fileDistance, rankDistance, distance


# Useful Constants

@docs all, a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, b3, c3, d3, e3, f3, g3, h3, a4, b4, c4, d4, e4, f4, g4, h4, a5, b5, c5, d5, e5, f5, g5, h5, a6, b6, c6, d6, e6, f6, g6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8

-}

import Internal.Square as Internal
import SquareFile exposing (SquareFile)
import SquareRank exposing (SquareRank)


{-| Type representing one of the 64 squares of the board.
-}
type alias Square =
    Internal.Square


{-| Create a square from a `SquareFile` and a `SquareRank`.
-}
make : SquareFile -> SquareRank -> Square
make =
    Internal.make


{-| The file of a square.
-}
file : Square -> SquareFile
file =
    Internal.file


{-| The rank of a square.
-}
rank : Square -> SquareRank
rank =
    Internal.rank


{-| Converts a `Square` to a `String` in standard algebraic notation

    toString e4 == "e4"

-}
toString : Square -> String
toString =
    Internal.toString


{-| Tries to convert a `String` to a `Square`, by looking at the first two
characters of the string. If the two first characters are not a valid square
string, returns `Nothing`.

    fromString "g8" == Just g8

    fromString "b1d3" == Just b1

    fromString "Donald Trump" == Nothing

-}
fromString : String -> Maybe Square
fromString =
    Internal.fromString


{-| Converts a `Square` to an `Int` in the 0..63 range, using the mapping
a1 <-> 0, b1 <-> 1, ..., h8 <-> 63.

    toInt a1 == 0

    toInt b1 == 1

    toInt a2 == 8

    toInt h8 == 63

-}
toInt : Square -> Int
toInt =
    Internal.compress


{-| Tries to convert an `Int` to a `Square`, using the mapping a1 <-> 0,
b1 <-> 1, ..., h8 <-> 63.

    fromInt 0 == Just a1

    fromInt 1 == Just b1

    fromInt 63 == Just h8

    fromInt -1 == Nothing

    fromInt 100 == Nothing

-}
fromInt : Int -> Maybe Square
fromInt i =
    if i < 0 || i > 63 then
        Nothing

    else
        Just (Internal.expand i)


{-| The horizontal distance between two squares.

    fileDistance a1 d3 == 3

    fileDistance c1 c7 == 0

    fileDistance h8 a3 == 7

-}
fileDistance : Square -> Square -> Int
fileDistance =
    Internal.fileDistance


{-| The vertical distance between two squares.

    rankDistance a1 b4 == 3

    rankDistance c2 h2 == 0

    rankDistance a8 c1 == 7

-}
rankDistance : Square -> Square -> Int
rankDistance =
    Internal.rankDistance


{-| The distance between two squares, measured as the maximum of the vertical
and horizontal distances, or the number of king moves required to get from
one square to the other on an empty board.

    distance a1 b4 == 3

    distance h8 b1 == 7

    distance f3 e4 == 1

-}
distance : Square -> Square -> Int
distance =
    Internal.distance


{-| A list of all squares on the board.
-}
all : List Square
all =
    Internal.all


{-| The a1 square.
-}
a1 : Square
a1 =
    Internal.a1


{-| The b1 square.
-}
b1 : Square
b1 =
    Internal.b1


{-| The c1 square.
-}
c1 : Square
c1 =
    Internal.c1


{-| The d1 square.
-}
d1 : Square
d1 =
    Internal.d1


{-| The e1 square.
-}
e1 : Square
e1 =
    Internal.e1


{-| The f1 square.
-}
f1 : Square
f1 =
    Internal.f1


{-| The g1 square.
-}
g1 : Square
g1 =
    Internal.g1


{-| The h1 square.
-}
h1 : Square
h1 =
    Internal.h1


{-| The a2 square.
-}
a2 : Square
a2 =
    Internal.a2


{-| The b2 square.
-}
b2 : Square
b2 =
    Internal.b2


{-| The c2 square.
-}
c2 : Square
c2 =
    Internal.c2


{-| The d2 square.
-}
d2 : Square
d2 =
    Internal.d2


{-| The e2 square.
-}
e2 : Square
e2 =
    Internal.e2


{-| The f2 square.
-}
f2 : Square
f2 =
    Internal.f2


{-| The g2 square.
-}
g2 : Square
g2 =
    Internal.g2


{-| The h2 square.
-}
h2 : Square
h2 =
    Internal.h2


{-| The a3 square.
-}
a3 : Square
a3 =
    Internal.a3


{-| The b3 square.
-}
b3 : Square
b3 =
    Internal.b3


{-| The c3 square.
-}
c3 : Square
c3 =
    Internal.c3


{-| The d3 square.
-}
d3 : Square
d3 =
    Internal.d3


{-| The e3 square.
-}
e3 : Square
e3 =
    Internal.e3


{-| The f3 square.
-}
f3 : Square
f3 =
    Internal.f3


{-| The g3 square.
-}
g3 : Square
g3 =
    Internal.g3


{-| The h3 square.
-}
h3 : Square
h3 =
    Internal.h3


{-| The a4 square.
-}
a4 : Square
a4 =
    Internal.a4


{-| The b4 square.
-}
b4 : Square
b4 =
    Internal.b4


{-| The c4 square.
-}
c4 : Square
c4 =
    Internal.c4


{-| The d4 square.
-}
d4 : Square
d4 =
    Internal.d4


{-| The e4 square.
-}
e4 : Square
e4 =
    Internal.e4


{-| The f4 square.
-}
f4 : Square
f4 =
    Internal.f4


{-| The g4 square.
-}
g4 : Square
g4 =
    Internal.g4


{-| The h4 square.
-}
h4 : Square
h4 =
    Internal.h4


{-| The a5 square.
-}
a5 : Square
a5 =
    Internal.a5


{-| The b5 square.
-}
b5 : Square
b5 =
    Internal.b5


{-| The c5 square.
-}
c5 : Square
c5 =
    Internal.c5


{-| The d5 square.
-}
d5 : Square
d5 =
    Internal.d5


{-| The e5 square.
-}
e5 : Square
e5 =
    Internal.e5


{-| The f5 square.
-}
f5 : Square
f5 =
    Internal.f5


{-| The g5 square.
-}
g5 : Square
g5 =
    Internal.g5


{-| The h5 square.
-}
h5 : Square
h5 =
    Internal.h5


{-| The a6 square.
-}
a6 : Square
a6 =
    Internal.a6


{-| The b6 square.
-}
b6 : Square
b6 =
    Internal.b6


{-| The c6 square.
-}
c6 : Square
c6 =
    Internal.c6


{-| The d6 square.
-}
d6 : Square
d6 =
    Internal.d6


{-| The e6 square.
-}
e6 : Square
e6 =
    Internal.e6


{-| The f6 square.
-}
f6 : Square
f6 =
    Internal.f6


{-| The g6 square.
-}
g6 : Square
g6 =
    Internal.g6


{-| The h6 square.
-}
h6 : Square
h6 =
    Internal.h6


{-| The a7 square.
-}
a7 : Square
a7 =
    Internal.a7


{-| The b7 square.
-}
b7 : Square
b7 =
    Internal.b7


{-| The c7 square.
-}
c7 : Square
c7 =
    Internal.c7


{-| The d7 square.
-}
d7 : Square
d7 =
    Internal.d7


{-| The e7 square.
-}
e7 : Square
e7 =
    Internal.e7


{-| The f7 square.
-}
f7 : Square
f7 =
    Internal.f7


{-| The g7 square.
-}
g7 : Square
g7 =
    Internal.g7


{-| The h7 square.
-}
h7 : Square
h7 =
    Internal.h7


{-| The a8 square.
-}
a8 : Square
a8 =
    Internal.a8


{-| The b8 square.
-}
b8 : Square
b8 =
    Internal.b8


{-| The c8 square.
-}
c8 : Square
c8 =
    Internal.c8


{-| The d8 square.
-}
d8 : Square
d8 =
    Internal.d8


{-| The e8 square.
-}
e8 : Square
e8 =
    Internal.e8


{-| The f8 square.
-}
f8 : Square
f8 =
    Internal.f8


{-| The g8 square.
-}
g8 : Square
g8 =
    Internal.g8


{-| The h8 square.
-}
h8 : Square
h8 =
    Internal.h8
