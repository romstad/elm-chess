module Internal.Square exposing (Square(..), a1, a2, a3, a4, a5, a6, a7, a8, add, all, b1, b2, b3, b4, b5, b6, b7, b8, c1, c2, c3, c4, c5, c6, c7, c8, compress, d1, d2, d3, d4, d5, d6, d7, d8, delta, deltasInDirection, distance, e1, e2, e3, e4, e5, e6, e7, e8, expand, f1, f2, f3, f4, f5, f6, f7, f8, file, fileDistance, fromString, g1, g2, g3, g4, g5, g6, g7, g8, h1, h2, h3, h4, h5, h6, h7, h8, isOutside, isRankTwo, make, possibleDeltasInDirection, rank, rankDistance, squaresInDirection, subtract, toString, unwrap)

import Internal.BoardDimensions exposing (..)
import Internal.PieceColor as PieceColor exposing (PieceColor, black, white)
import Internal.SquareDelta as SquareDelta exposing (SquareDelta(..))
import Internal.SquareFile as File exposing (SquareFile(..))
import Internal.SquareRank as Rank exposing (SquareRank(..))


{- Files, ranks and squares are all simple wrappers around Ints.
   Squares are used as indices into board arrays. The board array has a large
   frame of "outside" squares around it, in order to facilitate edge and attack
   detection.

   The squares on the extended board are situated like this:

   165 .. .. .. .. .. .. .. .. .. ..
   150 .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
   135 .. a8 b8 c8 d8 e8 f8 g8 h8 .. .. .. .. .. ..
   120 .. a7 b7 c7 d7 e7 f7 g7 h7 .. .. .. .. .. ..
   105 .. a6 b6 c6 d6 e6 f6 g6 h6 .. .. .. .. .. ..
    90 .. a5 b5 c5 d5 e5 f5 g5 h5 .. .. .. .. .. ..
    75 .. a4 b4 c4 d4 e4 f4 g4 h4 .. .. .. .. .. ..
    60 .. a3 b3 c3 d3 e3 f3 g3 h3 .. .. .. .. .. ..
    45 .. a2 b2 c2 d2 e2 f2 g2 h2 .. .. .. .. .. ..
    30 .. a1 b1 c1 d1 e1 f1 g1 h1 .. .. .. .. .. ..
    15 .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
     0 .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
-}


type Square
    = Square Int



{- Unwrap to Int, for shorter code. Should only be necessary in a few low level
   functions.
-}


unwrap : Square -> Int
unwrap square =
    case square of
        Square square_ ->
            square_



{- Test whether a square is outside the real board. This function should
   probably not be used much except for initialization purposes: When you have
   an actual Chess.Board or Chess.Position value, it's better to check that the
   piece on a particular square has the value Piece.outside.
-}


isOutside : Square -> Bool
isOutside square =
    File.isOutside (file square) || Rank.isOutside (rank square)



{- Create a square from a file and a rank. -}


make : SquareFile -> SquareRank -> Square
make file_ rank_ =
    Square (File.unwrap file_ + Rank.unwrap rank_ * extendedFileCount)



{- Extract files and ranks from a square -}


file : Square -> SquareFile
file square =
    SquareFile (modBy extendedFileCount (unwrap square))


rank : Square -> SquareRank
rank square =
    SquareRank (unwrap square // extendedFileCount)



{- Converting squares to/from strings. -}


toString : Square -> String
toString square =
    File.toString (file square) ++ Rank.toString (rank square)


fromString : String -> Maybe Square
fromString string =
    let
        f =
            File.fromString string

        r =
            Rank.fromString (String.dropLeft 1 string)
    in
        case f of
            Just f_ ->
                Maybe.map (make f_) r

            Nothing ->
                Nothing



{- Compress squares to ints in the 0..63 range, and expand back. -}


compress : Square -> Int
compress square =
    let
        f =
            modBy extendedFileCount (unwrap square)

        r =
            unwrap square // extendedFileCount
    in
        (f - fileMin) + fileCount * (r - rankMin)


expand : Int -> Square
expand i =
    let
        f =
            modBy fileCount i

        r =
            i // fileCount
    in
        Square (f + fileMin + (r + rankMin) * extendedFileCount)



{- Checks whether a square is on the second rank from the given side's
   point of view. Used when generating pawn pushes.
-}


isRankTwo : Square -> PieceColor -> Bool
isRankTwo square color =
    if color == white then
        (square |> rank |> Rank.unwrap) == rankMin + 1
    else
        (square |> rank |> Rank.unwrap) == rankMax - 1



{- Square delta arithmetics: -}


delta : Square -> Square -> SquareDelta
delta square0 square1 =
    SquareDelta (unwrap square0 - unwrap square1)


add : Square -> SquareDelta -> Square
add square delta_ =
    Square (unwrap square + SquareDelta.unwrap delta_)


subtract : Square -> Square -> SquareDelta
subtract square0 square1 =
    SquareDelta (unwrap square0 - unwrap square1)



{- The squares situated in the direction of the given square delta from the
   given starting square. This function is rather slow, and is only intended to
   be used for initialization functions.
-}


squaresInDirection : Square -> SquareDelta -> List Square
squaresInDirection startSquare delta_ =
    let
        squaresInDirectionInternal square acc =
            if isOutside square then
                acc
            else
                squaresInDirectionInternal
                    (add square delta_)
                    (square :: acc)
    in
        List.reverse <|
            squaresInDirectionInternal (add startSquare delta_) []



{- The deltas to the squares situated in the direction of the given square
   delta from the given starting square. This function is rather slow, and is
   only intended to be used for initialization purposes.
-}


deltasInDirection : Square -> SquareDelta -> List SquareDelta
deltasInDirection startSquare delta_ =
    List.map
        (\s -> subtract s startSquare)
        (squaresInDirection startSquare delta_)



{- The possible deltas in a given direction, if we start as far as possible
   from the edge of the board in that direction.
-}


possibleDeltasInDirection : SquareDelta -> List SquareDelta
possibleDeltasInDirection delta_ =
    List.foldl
        (\sq result ->
            let
                deltas =
                    deltasInDirection sq delta_
            in
                if List.length deltas > List.length result then
                    deltas
                else
                    result
        )
        []
        all



{- The horizontal distance between two squares. -}


fileDistance : Square -> Square -> Int
fileDistance square0 square1 =
    File.distance (file square1) (file square0)



{- The vertical distance between two squares. -}


rankDistance : Square -> Square -> Int
rankDistance square0 square1 =
    Rank.distance (rank square1) (rank square0)



{- The distance between two squares, measured as the maximum of the vertical
   and horizontal distances, or the number of king moves required to get from
   one square to the other on an empty board.
-}


distance : Square -> Square -> Int
distance square0 square1 =
    max (fileDistance square0 square1) (rankDistance square0 square1)



{- List of all squares on the board. -}


all : List Square
all =
    List.concatMap
        (\f -> List.map (make f) Rank.all)
        File.all



{- Square constants. -}


a1 : Square
a1 =
    make File.a Rank.one


b1 : Square
b1 =
    make File.b Rank.one


c1 : Square
c1 =
    make File.c Rank.one


d1 : Square
d1 =
    make File.d Rank.one


e1 : Square
e1 =
    make File.e Rank.one


f1 : Square
f1 =
    make File.f Rank.one


g1 : Square
g1 =
    make File.g Rank.one


h1 : Square
h1 =
    make File.h Rank.one


a2 : Square
a2 =
    make File.a Rank.two


b2 : Square
b2 =
    make File.b Rank.two


c2 : Square
c2 =
    make File.c Rank.two


d2 : Square
d2 =
    make File.d Rank.two


e2 : Square
e2 =
    make File.e Rank.two


f2 : Square
f2 =
    make File.f Rank.two


g2 : Square
g2 =
    make File.g Rank.two


h2 : Square
h2 =
    make File.h Rank.two


a3 : Square
a3 =
    make File.a Rank.three


b3 : Square
b3 =
    make File.b Rank.three


c3 : Square
c3 =
    make File.c Rank.three


d3 : Square
d3 =
    make File.d Rank.three


e3 : Square
e3 =
    make File.e Rank.three


f3 : Square
f3 =
    make File.f Rank.three


g3 : Square
g3 =
    make File.g Rank.three


h3 : Square
h3 =
    make File.h Rank.three


a4 : Square
a4 =
    make File.a Rank.four


b4 : Square
b4 =
    make File.b Rank.four


c4 : Square
c4 =
    make File.c Rank.four


d4 : Square
d4 =
    make File.d Rank.four


e4 : Square
e4 =
    make File.e Rank.four


f4 : Square
f4 =
    make File.f Rank.four


g4 : Square
g4 =
    make File.g Rank.four


h4 : Square
h4 =
    make File.h Rank.four


a5 : Square
a5 =
    make File.a Rank.five


b5 : Square
b5 =
    make File.b Rank.five


c5 : Square
c5 =
    make File.c Rank.five


d5 : Square
d5 =
    make File.d Rank.five


e5 : Square
e5 =
    make File.e Rank.five


f5 : Square
f5 =
    make File.f Rank.five


g5 : Square
g5 =
    make File.g Rank.five


h5 : Square
h5 =
    make File.h Rank.five


a6 : Square
a6 =
    make File.a Rank.six


b6 : Square
b6 =
    make File.b Rank.six


c6 : Square
c6 =
    make File.c Rank.six


d6 : Square
d6 =
    make File.d Rank.six


e6 : Square
e6 =
    make File.e Rank.six


f6 : Square
f6 =
    make File.f Rank.six


g6 : Square
g6 =
    make File.g Rank.six


h6 : Square
h6 =
    make File.h Rank.six


a7 : Square
a7 =
    make File.a Rank.seven


b7 : Square
b7 =
    make File.b Rank.seven


c7 : Square
c7 =
    make File.c Rank.seven


d7 : Square
d7 =
    make File.d Rank.seven


e7 : Square
e7 =
    make File.e Rank.seven


f7 : Square
f7 =
    make File.f Rank.seven


g7 : Square
g7 =
    make File.g Rank.seven


h7 : Square
h7 =
    make File.h Rank.seven


a8 : Square
a8 =
    make File.a Rank.eight


b8 : Square
b8 =
    make File.b Rank.eight


c8 : Square
c8 =
    make File.c Rank.eight


d8 : Square
d8 =
    make File.d Rank.eight


e8 : Square
e8 =
    make File.e Rank.eight


f8 : Square
f8 =
    make File.f Rank.eight


g8 : Square
g8 =
    make File.g Rank.eight


h8 : Square
h8 =
    make File.h Rank.eight
