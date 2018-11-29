module Move exposing
    ( Move, Variation
    , from, to, promotion, isPromotion, isCastle, isKingsideCastle, isQueensideCastle, isEp
    , toUci
    )

{-| The `Move` and `Variation` data types, and miscellaneus functions
operating on moves.


# Types

@docs Move, Variation


# Extracting Properties of Moves

@docs from, to, promotion, isPromotion, isCastle, isKingsideCastle, isQueensideCastle, isEp


# Converting Moves to UCI Notation

@docs toUci

-}

import Internal.Move as Internal
import PieceType exposing (PieceType)
import Square exposing (Square)


{-| Type representing a chess move. The move contains information about the
from and to square, and whether the move is a castle, an en passant capture,
or a pawn promotion.
-}
type alias Move =
    Internal.Move


{-| Type representing a variation, i.e. a sequence of moves.
-}
type alias Variation =
    List Move


{-| The source square of a move.
-}
from : Move -> Square
from =
    Internal.from


{-| The destination square of a move.
-}
to : Move -> Square
to =
    Internal.to


{-| The piece type to which the move promotes, or `Nothing` if the move is not
a promotion move.
-}
promotion : Move -> Maybe PieceType
promotion =
    Internal.promotion


{-| Whether the move is a pawn promotion
-}
isPromotion : Move -> Bool
isPromotion =
    Internal.isPromotion


{-| Whether the move is a castling move.
-}
isCastle : Move -> Bool
isCastle =
    Internal.isCastle


{-| Whether the move is a kingside castling move.
-}
isKingsideCastle : Move -> Bool
isKingsideCastle =
    Internal.isKingsideCastle


{-| Whether the move is a queenside castling move.
-}
isQueensideCastle : Move -> Bool
isQueensideCastle =
    Internal.isQueensideCastle


{-| Whether the move is an en passant capture.
-}
isEp : Move -> Bool
isEp =
    Internal.isEp


{-| Translate a move to UCI notation. Translating _from_ UCI cannot be done by
inspecting the move a lone, since it also requires a board.
-}
toUci : Move -> String
toUci =
    Internal.toUci
