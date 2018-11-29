module Internal.Position exposing (PosRec, Position(..), bishopPseudoMovesFrom, canCastleKingside, canCastleQueenside, colorOn, doMove, empty, epSquare, fromFen, initial, isCheck, isCheckmate, isEmpty, isInCheck, kingCastlePseudoMovesFrom, kingPseudoMovesFrom, kingSquare, knightPseudoMovesFrom, lastMove, lineIsClear, moveGivesCheck, moveGivesCheckmate, moveNumber, movePiece, moves, movesFrom, movesTo, parent, pawnCapturePseudoMovesTo, pawnCaptures, pawnEpCapturePseudoMoves, pawnEpCaptures, pawnPseudoMovesFrom, pawnPseudoMovesTo, pawnPushPseudoMovesTo, pawnPushes, pieceAttacksSquare, pieceOn, piecePseudoMovesTo, pinDirection, pseudoMoveIsLegal, pseudoMovesFrom, pseudoMovesTo, putPiece, queenPseudoMovesFrom, removePiece, rookPseudoMovesFrom, scan, sideAttacksSquare, sideToMove, slidePseudoMovesFrom, toFen, toUci, unwrap)

{- The `Position` type is a record representing a position from a chess game. -}

import Array exposing (Array)
import Internal.Board as Board exposing (Board)
import Internal.CastleRights as CastleRights exposing (CastleRights)
import Internal.Move as Move exposing (Move(..))
import Internal.Piece as Piece
    exposing
        ( Piece(..)
        , blackBishop
        , blackKing
        , blackKnight
        , blackPawn
        , blackQueen
        , blackRook
        , whiteBishop
        , whiteKing
        , whiteKnight
        , whitePawn
        , whiteQueen
        , whiteRook
        )
import Internal.PieceColor as PieceColor exposing (PieceColor, black, white)
import Internal.PieceType as PieceType
    exposing
        ( PieceType
        , bishop
        , king
        , knight
        , pawn
        , queen
        , rook
        )
import Internal.Square as Square exposing (Square(..))
import Internal.SquareDelta as Delta exposing (SquareDelta)


type alias PosRec =
    { board : Board -- The current board position
    , sideToMove : PieceColor -- The current side to move
    , whiteKingSquare : Maybe Square -- Square of white's king
    , blackKingSquare : Maybe Square -- Square of black's king
    , castleRights : CastleRights -- Current castle rights info
    , epSquare : Maybe Square -- En passant square, if available
    , rule50Counter : Int -- Moves since last capture or pawn push
    , gamePly : Int -- Number of half moves in the game so far
    , lastMove : Maybe Move -- The last move that was played
    , parent : Maybe Position -- The position before the last move
    }


type Position
    = Position PosRec



{- Unwrap to a plain record, for shorter code. Should only be necessary in a
   few low level functions.
-}


unwrap : Position -> PosRec
unwrap pos =
    case pos of
        Position pos_ ->
            pos_



{- An empty position. Note that this position is not a legal one, as it is
   lacking kings for both sides. Pieces have to be added before the position is
   usable.
-}


empty : Position
empty =
    Position
        { board = Board.empty
        , sideToMove = white
        , whiteKingSquare = Nothing
        , blackKingSquare = Nothing
        , castleRights = CastleRights.empty
        , epSquare = Nothing
        , rule50Counter = 0
        , gamePly = 0
        , lastMove = Nothing
        , parent = Nothing
        }


{-| The standard starting position.
-}
initial : Position
initial =
    Maybe.withDefault empty <|
        fromFen
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


{-| Get the current side to move.
-}
sideToMove : Position -> PieceColor
sideToMove position =
    (unwrap position).sideToMove


{-| En passant square, if available.
-}
epSquare : Position -> Maybe Square
epSquare position =
    (unwrap position).epSquare


{-| Whether the given side still have kingside castling rights.
-}
canCastleKingside : PieceColor -> Position -> Bool
canCastleKingside side position =
    CastleRights.canCastleKingside
        side
        (unwrap position).castleRights


{-| Whether the given side still have queenside castling rights.
-}
canCastleQueenside : PieceColor -> Position -> Bool
canCastleQueenside side position =
    CastleRights.canCastleQueenside
        side
        (unwrap position).castleRights


{-| Get the piece on the given square. In case of an out of range square
(which shouldn't be possible), we return `Piece.outside`.
-}
pieceOn : Square -> Position -> Piece
pieceOn square pos =
    Board.pieceOn square (unwrap pos).board


{-| The color of the piece on the given square.
-}
colorOn : Square -> Position -> PieceColor
colorOn square pos =
    Piece.color (pieceOn square pos)


{-| Tests whether the given square on the board is empty.
-}
isEmpty : Square -> Position -> Bool
isEmpty square pos =
    Board.isEmpty square (unwrap pos).board


{-| Current move number in the game.
-}
moveNumber : Position -> Int
moveNumber position =
    (unwrap position).gamePly // 2 + 1


{-| The square of the king for the given side.
-}
kingSquare : PieceColor -> Position -> Maybe Square
kingSquare color position =
    if color == white then
        (unwrap position).whiteKingSquare
    else
        (unwrap position).blackKingSquare


{-| The parent of a position, i.e. the position before the last move was made.
-}
parent : Position -> Maybe Position
parent position =
    (unwrap position).parent


{-| The last move made to reach this position
-}
lastMove : Position -> Maybe Move
lastMove position =
    (unwrap position).lastMove


{-| Put the given piece on the given square of the board, and return the
modified board.
-}
putPiece : Piece -> Square -> Position -> Position
putPiece piece square position =
    let
        pos =
            unwrap position
    in
        if piece == Piece.whiteKing then
            Position
                { pos
                    | board = Board.putPiece piece square pos.board
                    , whiteKingSquare = Just square
                }
        else if piece == Piece.blackKing then
            Position
                { pos
                    | board = Board.putPiece piece square pos.board
                    , blackKingSquare = Just square
                }
        else
            Position
                { pos | board = Board.putPiece piece square pos.board }


{-| Remove the piece on the given square of the board, and return the modified
board.
-}
removePiece : Square -> Position -> Position
removePiece square position =
    let
        pos =
            unwrap position
    in
        Position { pos | board = Board.removePiece square pos.board }


{-| Move a piece from one square to another, returing the modified position.
If the destination square is not empty, the piece on that square will vanish
without a trace.
-}
movePiece : Square -> Square -> Position -> Position
movePiece from to position =
    let
        pos =
            unwrap position

        piece =
            pieceOn from position
    in
        if piece == Piece.whiteKing then
            Position
                { pos
                    | board = Board.movePiece from to pos.board
                    , whiteKingSquare = Just to
                }
        else if piece == Piece.blackKing then
            Position
                { pos
                    | board = Board.movePiece from to pos.board
                    , blackKingSquare = Just to
                }
        else
            Position
                { pos | board = Board.movePiece from to pos.board }


{-| Scan the board from a given start square along a given direction (a square
delta, assumed to be one of the eight sliding direction) until a non-empty
or outside square is reached. Returns the first non-empty or outside square
found in this direction.
-}
scan : Position -> Square -> SquareDelta -> Square
scan position square delta =
    Board.scan (unwrap position).board square delta


{-| Checks that the ray between two colinear squares is clear from obstructions
along the given direction.
-}
lineIsClear : Position -> Square -> Square -> SquareDelta -> Bool
lineIsClear position from to delta =
    Board.lineIsClear (unwrap position).board from to delta


{-| Checks whether the piece on square `from` attacks square `to`
-}
pieceAttacksSquare : Square -> Square -> Position -> Bool
pieceAttacksSquare from to position =
    Board.pieceAttacksSquare from to (unwrap position).board


{-| Checks whether the given side attacks the given square.
-}
sideAttacksSquare : PieceColor -> Square -> Position -> Bool
sideAttacksSquare side square position =
    Board.sideAttacksSquare side square (unwrap position).board


{-| Checks whether the given side is in check.
-}
isInCheck : PieceColor -> Position -> Bool
isInCheck side position =
    case kingSquare side position of
        Nothing ->
            False

        Just kingSquare_ ->
            sideAttacksSquare
                (PieceColor.opposite side)
                kingSquare_
                position


{-| Checks whether the side to move is in check.
-}
isCheck : Position -> Bool
isCheck position =
    isInCheck (sideToMove position) position


{-| Checks whether the side to move is checkmated.
-}
isCheckmate : Position -> Bool
isCheckmate position =
    isCheck position && List.length (moves position) == 0


{-| Checks whether a move gives check.
-}
moveGivesCheck : Move -> Position -> Bool
moveGivesCheck move position =
    position |> doMove move |> isCheck


{-| Checks whether a move gives checkmate.
-}
moveGivesCheckmate : Move -> Position -> Bool
moveGivesCheckmate move position =
    position |> doMove move |> isCheckmate


{-| Direction in which the piece on the given square is pinned (a square
delta).
-}
pinDirection : Square -> Position -> Maybe SquareDelta
pinDirection square position =
    let
        color =
            Piece.color (pieceOn square position)

        kingSq =
            kingSquare color position
    in
        case kingSquare color position of
            Nothing ->
                Nothing

            Just kingSq_ ->
                case Piece.attackDelta Piece.whiteQueen square kingSq_ of
                    Nothing ->
                        Nothing

                    Just delta ->
                        if not (lineIsClear position square kingSq_ delta) then
                            Nothing
                        else
                            let
                                s =
                                    scan position square (Delta.negate delta)
                            in
                                case Piece.attackDelta (pieceOn s position) s kingSq_ of
                                    Nothing ->
                                        Nothing

                                    Just _ ->
                                        Just delta


{-| Do a move on the board, returning the resulting position.
-}
doMove : Move -> Position -> Position
doMove move position =
    let
        from =
            Move.from move

        to =
            Move.to move

        piece =
            pieceOn from position
    in
        case position of
            Position pos ->
                Position
                    { board = Board.doMove move pos.board
                    , sideToMove = PieceColor.opposite pos.sideToMove
                    , whiteKingSquare =
                        if piece == whiteKing then
                            Just to
                        else
                            pos.whiteKingSquare
                    , blackKingSquare =
                        if piece == blackKing then
                            Just to
                        else
                            pos.blackKingSquare
                    , castleRights = CastleRights.doMove move pos.castleRights
                    , epSquare =
                        if
                            (piece == whitePawn)
                                && (Square.subtract to from == Delta.nn)
                        then
                            Just (Square.add from Delta.n)
                        else if
                            (piece == blackPawn)
                                && (Square.subtract to from == Delta.ss)
                        then
                            Just (Square.add from Delta.s)
                        else
                            Nothing
                    , rule50Counter =
                        if
                            (Piece.kind piece == pawn)
                                || (pieceOn to position /= Piece.empty)
                        then
                            0
                        else
                            pos.rule50Counter + 1
                    , gamePly = pos.gamePly + 1
                    , lastMove = Just move
                    , parent = Just position
                    }


{-| List of all legal moves.
-}
moves : Position -> List Move
moves position =
    List.concatMap
        (\s -> movesFrom s position)
        Square.all


{-| Legal moves from the given square.
-}
movesFrom : Square -> Position -> List Move
movesFrom square position =
    List.filter
        (pseudoMoveIsLegal position)
        (pseudoMovesFrom square position)


{-| Pseudo-legal moves to the given square for the given piece type. Castling
moves are not included. This function is used for SAN move input and output.
-}
movesTo : PieceType -> Square -> Position -> List Move
movesTo piece square position =
    List.filter
        (pseudoMoveIsLegal position)
        (pseudoMovesTo piece square position)


{-| Pseudo-legal moves from the given square. This means any move that would be
possible if we don't take into account putting our own king in check.
-}
pseudoMovesFrom : Square -> Position -> List Move
pseudoMovesFrom square position =
    let
        piece =
            pieceOn square position
    in
        if Piece.color piece /= sideToMove position then
            []
        else if Piece.kind piece == pawn then
            pawnPseudoMovesFrom square position
        else if Piece.kind piece == knight then
            knightPseudoMovesFrom square position
        else if Piece.kind piece == bishop then
            bishopPseudoMovesFrom square position
        else if Piece.kind piece == rook then
            rookPseudoMovesFrom square position
        else if Piece.kind piece == queen then
            queenPseudoMovesFrom square position
        else if Piece.kind piece == king then
            kingPseudoMovesFrom square position
        else
            []


{-| Pseudo-legal moves to the given square for the given piece type. Castling
moves are not included. This function is used for SAN move input and output.
-}
pseudoMovesTo : PieceType -> Square -> Position -> List Move
pseudoMovesTo piece square position =
    let
        capturedPiece =
            pieceOn square position

        us =
            sideToMove position

        them =
            PieceColor.opposite us
    in
        if
            not
                (capturedPiece
                    == Piece.empty
                    || Piece.color capturedPiece
                    == them
                )
        then
            []
        else if piece == pawn then
            pawnPseudoMovesTo us square position
        else
            piecePseudoMovesTo us piece square position


{-| Test whether a pseudo-legal move is legal; i.e. that it does not leave the
player's own king in check.
-}
pseudoMoveIsLegal : Position -> Move -> Bool
pseudoMoveIsLegal position move =
    not (isInCheck (sideToMove position) (doMove move position))


{-| Initialize a position from a FEN string.
-}
fromFen : String -> Maybe Position
fromFen fen =
    let
        components =
            Array.fromList (String.split " " fen)

        board =
            Board.fromFen (Maybe.withDefault "" (Array.get 0 components))

        sideToMove_ =
            PieceColor.fromString
                (Maybe.withDefault "w" (Array.get 1 components))

        castleRights =
            CastleRights.fromString
                (Maybe.withDefault "-" (Array.get 2 components))

        epSquare_ =
            Square.fromString
                (Maybe.withDefault "-" (Array.get 3 components))

        whiteKingSquare =
            List.filter
                (\s -> Board.pieceOn s board == Piece.whiteKing)
                Square.all
                |> List.head

        blackKingSquare =
            List.filter
                (\s -> Board.pieceOn s board == Piece.blackKing)
                Square.all
                |> List.head
    in
        Just
            (Position
                { board = board
                , sideToMove = Maybe.withDefault white sideToMove_
                , whiteKingSquare = whiteKingSquare
                , blackKingSquare = blackKingSquare
                , castleRights = castleRights
                , epSquare = epSquare_
                , rule50Counter = 0
                , gamePly = 0
                , lastMove = Nothing
                , parent = Nothing
                }
            )


{-| Convert a position to a FEN string
-}
toFen : Position -> String
toFen position =
    let
        pos =
            unwrap position
    in
        Board.toFen pos.board
            ++ " "
            ++ PieceColor.toString pos.sideToMove
            ++ " "
            ++ CastleRights.toString pos.castleRights
            ++ " "
            ++ (case pos.epSquare of
                    Just s ->
                        Square.toString s

                    Nothing ->
                        "-"
               )
            ++ " "
            ++ String.fromInt pos.rule50Counter
            ++ " "
            ++ String.fromInt (pos.gamePly // 2 + 1)


{-| Convert a position to UCI notation, for sending it to an engine by a
`position fen <fen> moves ...` command.
-}
toUci : Position -> String
toUci position =
    let
        toUciHelper : Position -> List Move -> ( Position, List Move )
        toUciHelper pos moves_ =
            if (unwrap pos).rule50Counter == 0 then
                ( pos, moves_ )
            else
                case parent pos of
                    Nothing ->
                        ( pos, moves_ )

                    Just p ->
                        case lastMove pos of
                            Nothing ->
                                ( pos, moves_ )

                            Just move ->
                                toUciHelper p (move :: moves_)

        ( root, moves__ ) =
            toUciHelper position []
    in
        "position fen "
            ++ toFen root
            ++ (if List.length moves__ == 0 then
                    ""
                else
                    " moves "
                        ++ List.foldr
                            (++)
                            ""
                            (List.intersperse
                                " "
                                (List.map Move.toUci moves__)
                            )
               )



{- Move generation helper functions. -}


pawnPseudoMovesFrom : Square -> Position -> List Move
pawnPseudoMovesFrom square position =
    let
        us =
            sideToMove position

        them =
            PieceColor.opposite us
    in
        pawnPushes us them square position
            ++ pawnCaptures us them square position
            ++ pawnEpCaptures us them square position


pawnPushes : PieceColor -> PieceColor -> Square -> Position -> List Move
pawnPushes us them square position =
    let
        push =
            if us == white then
                Delta.n
            else
                Delta.s

        doublePush =
            Delta.multiply 2 push
    in
        if not (isEmpty (Square.add square push) position) then
            []
        else if Square.isRankTwo square them then
            List.map
                (Move.makePromotion square (Square.add square push))
                [ queen, rook, bishop, knight ]
        else if Square.isRankTwo square us then
            [ Move.make square (Square.add square push) ]
                ++ (if not (isEmpty (Square.add square doublePush) position) then
                        []
                    else
                        [ Move.make square (Square.add square doublePush) ]
                   )
        else
            [ Move.make square (Square.add square push) ]


pawnCaptures : PieceColor -> PieceColor -> Square -> Position -> List Move
pawnCaptures us them square position =
    let
        ds =
            if us == white then
                [ Delta.nw, Delta.ne ]
            else
                [ Delta.sw, Delta.se ]

        toSqs =
            ds
                |> List.map (Square.add square)
                |> List.filter (\s -> colorOn s position == them)
    in
        if Square.isRankTwo square them then
            List.concatMap
                (\to ->
                    List.map
                        (Move.makePromotion square to)
                        [ queen, rook, bishop, knight ]
                )
                toSqs
        else
            List.map (Move.make square) toSqs


pawnEpCaptures : PieceColor -> PieceColor -> Square -> Position -> List Move
pawnEpCaptures us them square position =
    case epSquare position of
        Nothing ->
            []

        Just epSquare_ ->
            let
                ds =
                    if us == white then
                        [ Delta.nw, Delta.ne ]
                    else
                        [ Delta.sw, Delta.se ]
            in
                List.map (Square.add square) ds
                    |> List.filter ((==) epSquare_)
                    |> List.map (Move.makeEp square)


knightPseudoMovesFrom : Square -> Position -> List Move
knightPseudoMovesFrom square position =
    let
        them =
            sideToMove position |> PieceColor.opposite
    in
        Piece.attackDirections whiteKnight
            |> List.map (Square.add square)
            |> List.filter
                (\s ->
                    isEmpty s position
                        || (Piece.color (pieceOn s position) == them)
                )
            |> List.map (\to -> Move.make square to)


kingPseudoMovesFrom : Square -> Position -> List Move
kingPseudoMovesFrom square position =
    let
        us =
            sideToMove position

        them =
            PieceColor.opposite us
    in
        (Piece.attackDirections whiteKing
            |> List.map (Square.add square)
            |> List.filter
                (\s ->
                    isEmpty s position
                        || (Piece.color (pieceOn s position) == them)
                )
            |> List.map (\to -> Move.make square to)
        )
            ++ kingCastlePseudoMovesFrom us them square position


kingCastlePseudoMovesFrom :
    PieceColor
    -> PieceColor
    -> Square
    -> Position
    -> List Move
kingCastlePseudoMovesFrom us them square position =
    (if canCastleKingside us position then
        let
            f1 =
                Square.add square Delta.e

            g1 =
                Square.add f1 Delta.e
        in
            if
                isEmpty f1 position
                    && isEmpty g1 position
                    && not (sideAttacksSquare them square position)
                    && not (sideAttacksSquare them f1 position)
                    && not (sideAttacksSquare them g1 position)
            then
                [ Move.makeCastle square g1 ]
            else
                []
     else
        []
    )
        ++ (if canCastleQueenside us position then
                let
                    d1 =
                        Square.add square Delta.w

                    c1 =
                        Square.add d1 Delta.w

                    b1 =
                        Square.add c1 Delta.w
                in
                    if
                        isEmpty d1 position
                            && isEmpty c1 position
                            && isEmpty b1 position
                            && not (sideAttacksSquare them square position)
                            && not (sideAttacksSquare them d1 position)
                            && not (sideAttacksSquare them c1 position)
                    then
                        [ Move.makeCastle square c1 ]
                    else
                        []
            else
                []
           )


bishopPseudoMovesFrom : Square -> Position -> List Move
bishopPseudoMovesFrom square position =
    List.concatMap
        (slidePseudoMovesFrom square position)
        (Piece.attackDirections whiteBishop)


rookPseudoMovesFrom : Square -> Position -> List Move
rookPseudoMovesFrom square position =
    List.concatMap
        (slidePseudoMovesFrom square position)
        (Piece.attackDirections whiteRook)


queenPseudoMovesFrom : Square -> Position -> List Move
queenPseudoMovesFrom square position =
    List.concatMap
        (slidePseudoMovesFrom square position)
        (Piece.attackDirections whiteQueen)


slidePseudoMovesFrom : Square -> Position -> SquareDelta -> List Move
slidePseudoMovesFrom from position delta =
    let
        us =
            sideToMove position

        them =
            PieceColor.opposite us

        slidePseudoMovesFromInternal to result =
            if isEmpty to position then
                slidePseudoMovesFromInternal
                    (Square.add to delta)
                    (Move.make from to :: result)
            else if (pieceOn to position |> Piece.color) == them then
                Move.make from to :: result
            else
                result
    in
        slidePseudoMovesFromInternal (Square.add from delta) []


piecePseudoMovesTo : PieceColor -> PieceType -> Square -> Position -> List Move
piecePseudoMovesTo us pieceType to position =
    let
        ourPiece =
            Piece.make us pieceType
    in
        Piece.attackDirections ourPiece
            |> List.map
                (if Piece.isSlider ourPiece then
                    scan position to
                 else
                    Square.add to
                )
            |> List.filter (\s -> pieceOn s position == ourPiece)
            |> List.map (\from -> Move.make from to)


pawnPseudoMovesTo : PieceColor -> Square -> Position -> List Move
pawnPseudoMovesTo us to position =
    pawnEpCapturePseudoMoves us to position
        ++ (if isEmpty to position then
                pawnPushPseudoMovesTo us to position
            else
                pawnCapturePseudoMovesTo us to position
           )


pawnPushPseudoMovesTo : PieceColor -> Square -> Position -> List Move
pawnPushPseudoMovesTo us to position =
    let
        them =
            PieceColor.opposite us

        ourPawn =
            Piece.make us pawn

        push =
            if us == white then
                Delta.s
            else
                Delta.n

        from =
            Square.add to push
    in
        if pieceOn from position == ourPawn then
            if Square.isRankTwo from them then
                List.map
                    (Move.makePromotion from to)
                    [ queen, rook, bishop, knight ]
            else
                [ Move.make from to ]
        else if isEmpty from position then
            let
                from2 =
                    Square.add from push
            in
                if
                    Square.isRankTwo from2 us
                        && (pieceOn from2 position == ourPawn)
                then
                    [ Move.make from2 to ]
                else
                    []
        else
            []


pawnCapturePseudoMovesTo : PieceColor -> Square -> Position -> List Move
pawnCapturePseudoMovesTo us to position =
    let
        them =
            PieceColor.opposite us

        ourPawn =
            Piece.make us pawn

        ds =
            if us == white then
                [ Delta.sw, Delta.se ]
            else
                [ Delta.nw, Delta.ne ]
    in
        ds
            |> List.map (Square.add to)
            |> List.filter (\s -> pieceOn s position == ourPawn)
            |> List.concatMap
                (\from ->
                    if Square.isRankTwo from them then
                        List.map
                            (Move.makePromotion from to)
                            [ queen, rook, bishop, knight ]
                    else
                        [ Move.make from to ]
                )


pawnEpCapturePseudoMoves : PieceColor -> Square -> Position -> List Move
pawnEpCapturePseudoMoves us to position =
    case epSquare position of
        Nothing ->
            []

        Just epSquare_ ->
            if epSquare_ /= to then
                []
            else
                let
                    ds =
                        if us == white then
                            [ Delta.sw, Delta.se ]
                        else
                            [ Delta.nw, Delta.ne ]

                    ourPawn =
                        Piece.make us pawn
                in
                    ds
                        |> List.map (Square.add to)
                        |> List.filter (\s -> pieceOn s position == ourPawn)
                        |> List.map (\from -> Move.makeEp from to)
