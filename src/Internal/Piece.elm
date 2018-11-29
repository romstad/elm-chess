module Internal.Piece exposing (Piece(..), all, attackDelta, attackDeltas, attackDirections, blackBishop, blackKing, blackKnight, blackPawn, blackQueen, blackRook, color, computeAttackDeltas, empty, fromChar, fromString, isSlider, kind, make, outside, toChar, toString, unwrap, whiteBishop, whiteKing, whiteKnight, whitePawn, whiteQueen, whiteRook)

{- A `Piece` is a simple wrapper around an Int. -}

import Array exposing (Array)
import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Char
import Internal.PieceColor as PC exposing (PieceColor(..), black, white)
import Internal.PieceType as PT
    exposing
        ( PieceType(..)
        , bishop
        , king
        , knight
        , pawn
        , queen
        , rook
        )
import Internal.Square as Square exposing (Square)
import Internal.SquareDelta as Delta exposing (SquareDelta)


type Piece
    = Piece Int



{- Unwrap to Int, for shorter code. Should only be necessary in a few low level
   functions.
-}


unwrap : Piece -> Int
unwrap piece =
    case piece of
        Piece piece_ ->
            piece_


{-| Create a piece with the given color and type.
-}
make : PieceColor -> PieceType -> Piece
make color_ kind_ =
    Piece (or (shiftLeftBy 3 (PC.unwrap color_)) (PT.unwrap kind_))



{- Constants for individual pieces. The special values `empty` and `outside`
   represent the "piece" on an empty square and on a square outside the board,
   respectively.
-}


whitePawn : Piece
whitePawn =
    make white pawn


whiteKnight : Piece
whiteKnight =
    make white knight


whiteBishop : Piece
whiteBishop =
    make white bishop


whiteRook : Piece
whiteRook =
    make white rook


whiteQueen : Piece
whiteQueen =
    make white queen


whiteKing : Piece
whiteKing =
    make white king


blackPawn : Piece
blackPawn =
    make black pawn


blackKnight : Piece
blackKnight =
    make black knight


blackBishop : Piece
blackBishop =
    make black bishop


blackRook : Piece
blackRook =
    make black rook


blackQueen : Piece
blackQueen =
    make black queen


blackKing : Piece
blackKing =
    make black king


empty : Piece
empty =
    make PC.empty PT.none


outside : Piece
outside =
    make PC.outside PT.none



{- Extract the color and the type of a piece. -}


color : Piece -> PieceColor
color piece =
    PieceColor (shiftRightBy 3 (unwrap piece))


kind : Piece -> PieceType
kind piece =
    PieceType (and (unwrap piece) 7)



{- Whether a piece is a sliding piece, i.e. whether it can move multiple
   squares in the same direction as long as the path is unobstructed.
-}


isSlider : Piece -> Bool
isSlider =
    kind >> PT.isSlider



{- The directions in which the individual pieces attack. -}


attackDirections : Piece -> List SquareDelta
attackDirections piece =
    if piece == whitePawn then
        [ Delta.nw, Delta.ne ]
    else if piece == whiteKnight then
        [ Delta.nnw, Delta.nne, Delta.nww, Delta.nee, Delta.ssw, Delta.sse, Delta.sww, Delta.see ]
    else if piece == whiteBishop then
        [ Delta.nw, Delta.ne, Delta.sw, Delta.se ]
    else if piece == whiteRook then
        [ Delta.n, Delta.s, Delta.w, Delta.e ]
    else if piece == whiteQueen then
        [ Delta.nw, Delta.ne, Delta.sw, Delta.se, Delta.n, Delta.s, Delta.w, Delta.e ]
    else if piece == whiteKing then
        [ Delta.nw, Delta.ne, Delta.sw, Delta.se, Delta.n, Delta.s, Delta.w, Delta.e ]
    else if piece == blackPawn then
        [ Delta.sw, Delta.se ]
    else if piece == blackKnight then
        [ Delta.nnw, Delta.nne, Delta.nww, Delta.nee, Delta.ssw, Delta.sse, Delta.sww, Delta.see ]
    else if piece == blackBishop then
        [ Delta.nw, Delta.ne, Delta.sw, Delta.se ]
    else if piece == blackRook then
        [ Delta.n, Delta.s, Delta.w, Delta.e ]
    else if piece == blackQueen then
        [ Delta.nw, Delta.ne, Delta.sw, Delta.se, Delta.n, Delta.s, Delta.w, Delta.e ]
    else if piece == blackKing then
        [ Delta.nw, Delta.ne, Delta.sw, Delta.se, Delta.n, Delta.s, Delta.w, Delta.e ]
    else
        []



{- The direction in which the piece, situated on the `from` square, would have
   to move in order to attack the `to` square.
-}


attackDelta : Piece -> Square -> Square -> Maybe SquareDelta
attackDelta piece from to =
    let
        deltaMax =
            Delta.unwrap Delta.max
    in
        Maybe.withDefault
            Nothing
            (Array.get
                (unwrap piece
                    * (2 * deltaMax + 1)
                    + Square.unwrap to
                    - Square.unwrap from
                    + deltaMax
                )
                attackDeltas
            )



{- Conversion of pieces to and from characters and strings. White pieces are
   translated to uppercase letters, black pieces to lowercase letters. Useful
   for reading and writing FEN strings.
-}


toChar : Piece -> Char
toChar piece =
    if color piece == PC.white then
        Char.toUpper (PT.toChar (kind piece))
    else
        Char.toLower (PT.toChar (kind piece))


toString : Piece -> String
toString =
    toChar >> String.fromChar


fromChar : Char -> Maybe Piece
fromChar char =
    Maybe.map
        (make
            (if Char.isUpper char then
                white
             else
                black
            )
        )
        (PT.fromChar char)


fromString : String -> Maybe Piece
fromString string =
    Maybe.andThen fromChar (List.head (String.toList string))



{- List of all "real" pieces. -}


all : List Piece
all =
    [ whitePawn
    , whiteKnight
    , whiteBishop
    , whiteRook
    , whiteQueen
    , whiteKing
    , blackPawn
    , blackKnight
    , blackBishop
    , blackRook
    , blackQueen
    , blackKing
    ]



{- Internal helper functions for computing attacks. -}


computeAttackDeltas : Piece -> List (Maybe SquareDelta)
computeAttackDeltas piece =
    let
        deltasByDirection =
            List.concatMap
                (\d ->
                    if isSlider piece then
                        List.map
                            (\d2 -> ( d, d2 ))
                            (Square.possibleDeltasInDirection d)
                    else
                        [ ( d, d ) ]
                )
                (attackDirections piece)

        deltaMax =
            Delta.unwrap Delta.max
    in
        Array.toList <|
            List.foldl
                (\( d0, d ) result ->
                    Array.set
                        (deltaMax + Delta.unwrap d)
                        (Just d0)
                        result
                )
                (Array.repeat (2 * deltaMax + 1) Nothing)
                deltasByDirection


attackDeltas : Array (Maybe SquareDelta)
attackDeltas =
    Array.fromList <|
        List.concatMap
            computeAttackDeltas
            (List.map Piece (List.range 0 (unwrap blackKing)))
