module Notation exposing (fromSan, toSan, variationToSan, fromUci, toUci)

{-| Functions for converting moves and variations to and from textual
representations.


# Functions

@docs fromSan, toSan, variationToSan, fromUci, toUci

-}

import Internal.Notation as Internal
import Move exposing (Move, Variation)
import Position exposing (Position)


{-| Tries to convert a move string in short algebraic notation to a move.
Returns `Nothing` on failure.
-}
fromSan : String -> Position -> Maybe Move
fromSan =
    Internal.fromSan


{-| Converts a move to a string in short algebraic notation .
-}
toSan : Move -> Position -> String
toSan =
    Internal.toSan


{-| Exports a variation to a string with move numbers and moves in short
algebraic notation.
-}
variationToSan : Variation -> Position -> String
variationToSan =
    Internal.variationToSan


{-| Tries to convert a move string in Universal Chess Interface notation to a
move. Returns Nothing on failure.
-}
fromUci : String -> Position -> Maybe Move
fromUci =
    Internal.fromUci


{-| Convert a move to a string in Universal Chess Interface notation.
-}
toUci : Move -> String
toUci =
    Internal.toUci
