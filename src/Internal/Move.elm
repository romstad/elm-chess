module Internal.Move exposing (..)

{- A `Move` is a simple wrapper around an Int. -}

import Bitwise exposing (..)
import Internal.PieceType as PieceType exposing (PieceType(PieceType))
import Internal.Square as Square exposing (Square(Square))


type Move
    = Move Int



{- Variation: A list of moves. -}


type alias Variation =
    List Move



{- Unwrap to Int, for shorter code. Should only be necessary in a few low level
   functions.
-}


unwrap : Move -> Int
unwrap move =
    case move of
        Move move ->
            move



{- Functions for constructing moves. -}


make : Square -> Square -> Move
make from to =
    Move
        (or
            (Square.compress to)
            (shiftLeftBy 6 (Square.compress from))
        )


makePromotion : Square -> Square -> PieceType -> Move
makePromotion from to promotion =
    Move
        (or
            (or
                (Square.compress to)
                (shiftLeftBy 6 (Square.compress from))
            )
            (shiftLeftBy 12 (PieceType.unwrap promotion))
        )


makeCastle : Square -> Square -> Move
makeCastle from to =
    Move
        (or
            (or
                (Square.compress to)
                (shiftLeftBy 6 (Square.compress from))
            )
            (shiftLeftBy 15 1)
        )


makeEp : Square -> Square -> Move
makeEp from to =
    Move
        (or
            (or
                (Square.compress to)
                (shiftLeftBy 6 (Square.compress from))
            )
            (shiftLeftBy 16 1)
        )



{- Functions for extracting properties of moves. -}


to : Move -> Square
to move =
    Square.expand (and (unwrap move) 63)


from : Move -> Square
from move =
    Square.expand (and (shiftRightBy 6 (unwrap move)) 63)


promotion : Move -> Maybe PieceType
promotion move =
    let
        p =
            PieceType (and (shiftRightBy 12 (unwrap move)) 7)
    in
        if p == PieceType.none then
            Nothing
        else
            Just p


isPromotion : Move -> Bool
isPromotion move =
    promotion move /= Nothing


isCastle : Move -> Bool
isCastle move =
    and (shiftLeftBy 15 1) (unwrap move) /= 0


isKingsideCastle : Move -> Bool
isKingsideCastle move =
    isCastle move && (Square.unwrap (from move) < Square.unwrap (to move))


isQueensideCastle : Move -> Bool
isQueensideCastle move =
    isCastle move && (Square.unwrap (from move) > Square.unwrap (to move))


isEp : Move -> Bool
isEp move =
    and (shiftLeftBy 16 1) (unwrap move) /= 0



{- Translate a move to UCI notation. Translating _from_ UCI, or translating to
   or from moves, cannot be done by inspecting the move alone, since they also
   require a board.
-}


toUci : Move -> String
toUci move =
    case (promotion move) of
        Nothing ->
            Square.toString (from move)
                ++ Square.toString (to move)

        Just p ->
            Square.toString (from move)
                ++ Square.toString (to move)
                ++ String.toLower (PieceType.toString p)
