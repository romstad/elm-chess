module Internal.SquareRank exposing (SquareRank(..), all, distance, eight, five, four, fromChar, fromString, isOutside, one, seven, six, three, toChar, toIndex, toString, two, unwrap)

{- A `SquareRank` is a simple wrapper around an `Int`. -}

import Char
import Internal.BoardDimensions exposing (..)


type SquareRank
    = SquareRank Int



{- Unwrap to Int, for shorter code. Should only be necessary in a few low level
   functions.
-}


unwrap : SquareRank -> Int
unwrap rank =
    case rank of
        SquareRank rank_ ->
            rank_



{- Test whether a rank is outside the real board. -}


isOutside : SquareRank -> Bool
isOutside rank =
    unwrap rank < rankMin || unwrap rank > rankMax



{- Convert to and from strings and characters. -}


toChar : SquareRank -> Char
toChar rank =
    Char.fromCode (unwrap rank - rankMin + Char.toCode '1')


toString : SquareRank -> String
toString =
    toChar >> String.fromChar


fromChar : Char -> Maybe SquareRank
fromChar char =
    let
        r =
            Char.toCode char - Char.toCode '1'
    in
        if r >= 0 && r < rankCount then
            Just (SquareRank (r + rankMin))
        else
            Nothing


fromString : String -> Maybe SquareRank
fromString string =
    Maybe.andThen fromChar (List.head (String.toList string))



{- List of all ranks on the board. -}


all : List SquareRank
all =
    List.map SquareRank (List.range rankMin rankMax)



{- The vertical distance between two ranks. -}


distance : SquareRank -> SquareRank -> Int
distance rank0 rank1 =
    abs (unwrap rank1) - unwrap rank0



{- Convert a file to an index in the range 0 (for the 'a' file) to 7 (for the
   'h' file). Useful when drawing the chess board in SVG, among other things.
-}


toIndex : SquareRank -> Int
toIndex rank =
    unwrap rank - rankMin



{- Constants for individual ranks. -}


one : SquareRank
one =
    SquareRank rankMin


two : SquareRank
two =
    SquareRank (rankMin + 1)


three : SquareRank
three =
    SquareRank (rankMin + 2)


four : SquareRank
four =
    SquareRank (rankMin + 3)


five : SquareRank
five =
    SquareRank (rankMin + 4)


six : SquareRank
six =
    SquareRank (rankMin + 5)


seven : SquareRank
seven =
    SquareRank (rankMin + 6)


eight : SquareRank
eight =
    SquareRank (rankMin + 7)
