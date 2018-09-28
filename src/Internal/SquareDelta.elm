module Internal.SquareDelta exposing (SquareDelta(..), add, e, max, multiply, n, ne, nee, negate, nn, nne, nnw, nw, nww, pawnPush, s, se, see, ss, sse, ssw, subtract, sw, sww, unwrap, w, zero)

import Internal.BoardDimensions exposing (..)
import Internal.PieceColor exposing (..)


{- SquareDelta is essentially the vector between two squares. It's implemented
   as a plain wrapper around Ints, for performance reasons.
-}


type SquareDelta
    = SquareDelta Int



{- Unwrapping function, for code brevity. Should only be used in a few low-level
   functions.
-}


unwrap : SquareDelta -> Int
unwrap delta =
    case delta of
        SquareDelta delta_ ->
            delta_



-- Arithmetics: Addition, subtraction, and scalar multiplication.


add : SquareDelta -> SquareDelta -> SquareDelta
add delta0 delta1 =
    SquareDelta (unwrap delta0 + unwrap delta1)


subtract : SquareDelta -> SquareDelta -> SquareDelta
subtract delta0 delta1 =
    SquareDelta (unwrap delta0 - unwrap delta1)


multiply : Int -> SquareDelta -> SquareDelta
multiply i delta =
    SquareDelta (i * unwrap delta)


negate : SquareDelta -> SquareDelta
negate delta =
    multiply (Basics.negate 1) delta



-- Some useful square deltas


zero : SquareDelta
zero =
    SquareDelta 0


n : SquareDelta
n =
    SquareDelta extendedFileCount


s : SquareDelta
s =
    negate n


e : SquareDelta
e =
    SquareDelta 1


w : SquareDelta
w =
    negate e


nw : SquareDelta
nw =
    add n w


ne : SquareDelta
ne =
    add n e


sw : SquareDelta
sw =
    add s w


se : SquareDelta
se =
    add s e


nnw : SquareDelta
nnw =
    add (multiply 2 n) w


nne : SquareDelta
nne =
    add (multiply 2 n) e


ssw : SquareDelta
ssw =
    add (multiply 2 s) w


sse : SquareDelta
sse =
    add (multiply 2 s) e


nww : SquareDelta
nww =
    add n (multiply 2 w)


nee : SquareDelta
nee =
    add n (multiply 2 e)


sww : SquareDelta
sww =
    add s (multiply 2 w)


see : SquareDelta
see =
    add s (multiply 2 e)


nn : SquareDelta
nn =
    add n n


ss : SquareDelta
ss =
    add s s


max : SquareDelta
max =
    SquareDelta (squareMax - squareMin)


pawnPush : PieceColor -> SquareDelta
pawnPush color =
    if color == white then
        n
    else
        s
