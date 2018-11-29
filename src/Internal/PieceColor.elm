module Internal.PieceColor exposing (PieceColor(..), all, black, empty, fromChar, fromString, opposite, outside, toChar, toString, unwrap, white)

import Bitwise


type PieceColor
    = PieceColor Int


{-| Unwrap a PieceColor to an Int. Should only be necessary in a few low level
functions. You should almost certainly not use this in any external code.

    unwrap white == 0

    unwrap black == 1

-}
unwrap : PieceColor -> Int
unwrap color =
    case color of
        PieceColor color_ ->
            color_


{-| The color of a white piece.
-}
white : PieceColor
white =
    PieceColor 0


{-| The color of a black piece.
-}
black : PieceColor
black =
    PieceColor 1


{-| The "color" of an empty square. Neither black nor white.
-}
empty : PieceColor
empty =
    PieceColor 2


{-| The "color" of the contents of a square outside the real board.
Used when detecting the edges of the board when generating legal moves
and other low level tasks. Should rarely or never be used in high level
code.
-}
outside : PieceColor
outside =
    PieceColor 3


{-| List of all "real" piece colors, i.e. white and black
-}
all : List PieceColor
all =
    [ white, black ]


{-| The opposite of a color. Only works for "true" colors, i.e. white and
black.

    opposite white == black

    opposite black == white

-}
opposite : PieceColor -> PieceColor
opposite =
    unwrap >> Bitwise.xor 1 >> PieceColor


{-| Convert a PieceColor to a char of the form used when representing a board
in Forsyth-Edwards notation.

    toChar white == 'w'

    toChar black == 'b'

-}
toChar : PieceColor -> Char
toChar color =
    if color == white then
        'w'
    else if color == black then
        'b'
    else
        '-'


{-| Convert a PieceColor to a string of the form used when representing a
board in Forsyth-Edwards notation.

    toChar white == "w"

    toChar black == "b"

-}
toString : PieceColor -> String
toString =
    toChar >> String.fromChar


{-| Tries to convert a character to a PieceColor, using Forsyth-Edwards
encoding.
fromChar 'w' == Just white
fromChar 'b' == Just black
fromChar ch == Nothing -- for all ch not equal to 'w' or 'b'
-}
fromChar : Char -> Maybe PieceColor
fromChar char =
    if char == 'w' then
        Just white
    else if char == 'b' then
        Just black
    else
        Nothing


{-| Tries to convert a string to a PieceColor, using Forsyth-Edwards
encoding.
fromString "w" == Just white
fromString "b" == Just black
fromString str == Nothing -- for all str not starting with "w" or "b"
-}
fromString : String -> Maybe PieceColor
fromString string =
    Maybe.andThen fromChar (List.head (String.toList string))
