module Internal.BoardDimensions exposing
    ( fileCount, rankCount, fileMin, fileMax, rankMin, rankMax
    , squareMin, squareMax, extendedFileCount, extendedBoardSize
    )

{-| This module contains various integer constants specifying the size of
the chess board and various integer arrays used for move generation,
attack generation, and so on.

@docs fileCount, rankCount, fileMin, fileMax, rankMin, rankMax
@docs squareMin, squareMax, extendedFileCount, extendedBoardSize

-}


{-| The number of files on the board
-}
fileCount : Int
fileCount =
    8


{-| The number of ranks on the board
-}
rankCount : Int
rankCount =
    8


{-| The index of the first file of the board.
-}
fileMin : Int
fileMin =
    1


{-| The index of the first rank of the board.
-}
rankMin : Int
rankMin =
    2


{-| The index of the last file of the board.
-}
fileMax : Int
fileMax =
    fileMin + fileCount - 1


{-| The index of the last rank of the board.
-}
rankMax : Int
rankMax =
    rankMin + rankCount - 1


{-| The index of the first square of the board, i.e. the a1 square.
-}
squareMin : Int
squareMin =
    fileMin + rankMin * extendedFileCount


{-| The index of the last square of the board, i.e. the h8 square for a
standard 8x8 board.
-}
squareMax : Int
squareMax =
    fileMax + rankMax * extendedFileCount


{-| The number of files on the "extended" board, containing a wide frame on
all sides in order to assist us in edge detection.
-}
extendedFileCount : Int
extendedFileCount =
    2 * fileCount - 1


{-| The number of squares on the "extended" board, containing a wide frame
on all sides to assist us in edge detection.
-}
extendedBoardSize : Int
extendedBoardSize =
    (rankCount + 3) * extendedFileCount + fileCount + 2
