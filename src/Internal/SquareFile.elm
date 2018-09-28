module Internal.SquareFile exposing (SquareFile(..), a, all, b, c, d, distance, e, f, fromChar, fromString, g, h, isOutside, toChar, toIndex, toString, unwrap)

{- A `SquareFile` is a simple wrapper around an `Int`. -}

import Char
import Internal.BoardDimensions exposing (..)


type SquareFile
    = SquareFile Int



{- Unwrap to Int, for shorter code. Should only be necessary in a few low level
   functions.
-}


unwrap : SquareFile -> Int
unwrap file =
    case file of
        SquareFile file_ ->
            file_



{- Test whether a file is outside the real board. -}


isOutside : SquareFile -> Bool
isOutside file =
    unwrap file < fileMin || unwrap file > fileMax



{- Convert to and from strings and characters. -}


toChar : SquareFile -> Char
toChar file =
    Char.fromCode (unwrap file - fileMin + Char.toCode 'a')


toString : SquareFile -> String
toString =
    toChar >> String.fromChar


fromChar : Char -> Maybe SquareFile
fromChar char =
    let
        f_ =
            Char.toCode char - Char.toCode 'a'
    in
        if f_ >= 0 && f_ < fileCount then
            Just (SquareFile (f_ + fileMin))
        else
            Nothing


fromString : String -> Maybe SquareFile
fromString string =
    Maybe.andThen fromChar (List.head (String.toList string))



{- List of all files on the board. -}


all : List SquareFile
all =
    List.map SquareFile (List.range fileMin fileMax)



{- The horizontal distance between two files. -}


distance : SquareFile -> SquareFile -> Int
distance file0 file1 =
    abs (unwrap file1) - unwrap file0



{- Convert a file to an index in the range 0 (for the 'a' file) to 7 (for the
   'h' file). Useful when drawing the chess board in SVG, among other things.
-}


toIndex : SquareFile -> Int
toIndex file =
    unwrap file - fileMin



{- Constants for individual files. -}


a : SquareFile
a =
    SquareFile fileMin


b : SquareFile
b =
    SquareFile (fileMin + 1)


c : SquareFile
c =
    SquareFile (fileMin + 2)


d : SquareFile
d =
    SquareFile (fileMin + 3)


e : SquareFile
e =
    SquareFile (fileMin + 4)


f : SquareFile
f =
    SquareFile (fileMin + 5)


g : SquareFile
g =
    SquareFile (fileMin + 6)


h : SquareFile
h =
    SquareFile (fileMin + 7)
