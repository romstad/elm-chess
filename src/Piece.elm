module Piece exposing
    ( Piece
    , make, color, kind
    , all, whitePawn, whiteKnight, whiteBishop, whiteRook, whiteQueen, whiteKing, blackPawn, blackKnight, blackBishop, blackRook, blackQueen, blackKing
    , fromChar, fromString, toChar, toString
    )

{-|


# Types

@docs Piece


# Creating and Manipulating Pieces

@docs make, color, kind


# Useful Constants

@docs all, whitePawn, whiteKnight, whiteBishop, whiteRook, whiteQueen, whiteKing, blackPawn, blackKnight, blackBishop, blackRook, blackQueen, blackKing


# Converting Pieces to/from Chars and Strings

@docs fromChar, fromString, toChar, toString

-}

import Internal.Piece as Internal
import PieceColor exposing (PieceColor)
import PieceType exposing (PieceType)


{-| Type representing a chess piece, with a color (white or black) and a type
(pawn, bishop, knight, etc.).
-}
type alias Piece =
    Internal.Piece


{-| Create a piece with the given color and type.

    make PieceColor.white PieceType.queen == whiteQueen

    make PieceColor.black PieceType.knight == blackKnight

-}
make : PieceColor -> PieceType -> Piece
make =
    Internal.make


{-| Get the color of a piece.

    color whiteBishop == PieceColor.white

    color blackQueen == PieceColor.black

-}
color : Piece -> PieceColor
color =
    Internal.color


{-| Get the type of a piece. Unfortunately, the name `type` is taken, so we
have to use an awkward name.

    kind whiteBishop == PieceType.bishop

    kind blackQueen == PieceType.queen

-}
kind : Piece -> PieceType
kind =
    Internal.kind


{-| Converts a piece to its corresponding character in Forsyth-Edwards notation.
White pieces become uppercase letters, black pieces become lowercase letters.

    toChar whiteRook == 'R'

    toChar blackQueen == 'q'

-}
toChar : Piece -> Char
toChar =
    Internal.toChar


{-| Converts a piece to a single-character string containing the pices's
corresponding character in Forsyth-Edwards notation. White pieces become
uppercase letters, black pieces become lowercase letters.

    toString whiteRook == "R"

    toString blackQueen == "q"

-}
toString : Piece -> String
toString =
    Internal.toString


{-| Tries to convert a character to a piece, using Forsyth-Edwards notation.
Uppercase letters become white pieces, lowercase letters become black pieces.
If the character is not a valid piece letter, returns `Nothing`.

    fromChar 'R' == Just whiteRook

    fromChar 'q' == Just blackQueen

    fromChar 'x' == Nothing

-}
fromChar : Char -> Maybe Piece
fromChar =
    Internal.fromChar


{-| Tries to convert a string to a piece by inspecting the first character and
using Forsyth-Edwards notation. Uppercase letters become white pieces, lowercase
letters become black pieces. If the character is not a valid piece letter, or
if the string is empty, returns `Nothing`.

    fromString "R" == Just whiteRook

    fromString "Kbb" == Just whiteKing

    fromString "q" == Just blackQueen

    fromString "x" == Nothing

-}
fromString : String -> Maybe Piece
fromString =
    Internal.fromString


{-| List of all pieces
-}
all : List Piece
all =
    Internal.all


{-| A white pawn.
-}
whitePawn : Piece
whitePawn =
    Internal.whitePawn


{-| A white knight.
-}
whiteKnight : Piece
whiteKnight =
    Internal.whiteKnight


{-| A white bishop.
-}
whiteBishop : Piece
whiteBishop =
    Internal.whiteBishop


{-| A white rook.
-}
whiteRook : Piece
whiteRook =
    Internal.whiteRook


{-| A white queen.
-}
whiteQueen : Piece
whiteQueen =
    Internal.whiteQueen


{-| A white king.
-}
whiteKing : Piece
whiteKing =
    Internal.whiteKing


{-| A black pawn.
-}
blackPawn : Piece
blackPawn =
    Internal.blackPawn


{-| A black knight.
-}
blackKnight : Piece
blackKnight =
    Internal.blackKnight


{-| A black bishop.
-}
blackBishop : Piece
blackBishop =
    Internal.blackBishop


{-| A black rook.
-}
blackRook : Piece
blackRook =
    Internal.blackRook


{-| A black queen.
-}
blackQueen : Piece
blackQueen =
    Internal.blackQueen


{-| A black king.
-}
blackKing : Piece
blackKing =
    Internal.blackKing
