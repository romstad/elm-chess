module Internal.Notation exposing (fromSan, fromUci, toSan, toUci, variationToSan)

import Internal.Move as Move exposing (Move, Variation)
import Internal.Piece as Piece exposing (Piece)
import Internal.PieceColor as PieceColor exposing (black, white)
import Internal.PieceType as PieceType exposing (PieceType)
import Internal.Position as Position exposing (Position)
import Internal.Square as Square exposing (Square)
import Internal.SquareFile as File exposing (SquareFile)
import Internal.SquareRank as Rank exposing (SquareRank)


{-| Converts a move string in short algebraic notation to a move.
-}
fromSan : String -> Position -> Maybe Move
fromSan string position =
    let
        str =
            String.filter isImportantCharacter string
    in
        if str == "O-O" then
            kingsideCastlingFromSan position
        else if str == "O-O-O" then
            queensideCastlingFromSan position
        else if isPieceCharacter (String.left 1 str) then
            pieceMoveFromSan str position
        else
            pawnMoveFromSan str position


{-| Converts a move to a string in short algebraic notation .
-}
toSan : Move -> Position -> String
toSan move position =
    (if Move.isKingsideCastle move then
        "O-O"
     else if Move.isQueensideCastle move then
        "O-O-O"
     else
        let
            piece =
                Position.pieceOn (Move.from move) position
        in
            if Piece.kind piece == PieceType.pawn then
                pawnMoveToSan move position
            else
                pieceMoveToSan move position
    )
        ++ (if Position.moveGivesCheckmate move position then
                "#"
            else if Position.moveGivesCheck move position then
                "+"
            else
                ""
           )


{-| Exports a variation to a string with move numbers and moves in short
algebraic notation.
-}
variationToSan : Variation -> Position -> String
variationToSan variation position =
    (if Position.sideToMove position == black then
        String.fromInt (Position.moveNumber position) ++ "... "
     else
        ""
    )
        ++ (Tuple.first <|
                List.foldl
                    (\move ( str, pos ) ->
                        ( str
                            ++ " "
                            ++ (if Position.sideToMove pos == white then
                                    String.fromInt (Position.moveNumber pos) ++ ". "
                                else
                                    ""
                               )
                            ++ toSan move pos
                        , Position.doMove move pos
                        )
                    )
                    ( "", position )
                    variation
           )


{-| Convert a move string in Universal Chess Interface notation to a move.
-}
fromUci : String -> Position -> Maybe Move
fromUci string position =
    let
        from =
            Square.fromString string

        to =
            Square.fromString (String.dropLeft 2 string)

        promotion =
            PieceType.fromString (String.dropLeft 4 string)
    in
        case from of
            Nothing ->
                Nothing

            Just from_ ->
                case to of
                    Nothing ->
                        Nothing

                    Just to_ ->
                        findMove from_ to_ promotion position


{-| Convert a move to a string in Universal Chess Interface notation.
-}
toUci : Move -> String
toUci =
    Move.toUci



{- Various internal helper functions -}


pieceMoveToSan : Move -> Position -> String
pieceMoveToSan move position =
    let
        piece =
            Position.pieceOn (Move.from move) position
    in
        String.toUpper (Piece.toString piece)
            ++ disambiguation piece move position
            ++ (if Position.isEmpty (Move.to move) position then
                    ""
                else
                    "x"
               )
            ++ Square.toString (Move.to move)


pawnMoveToSan : Move -> Position -> String
pawnMoveToSan move position =
    let
        from =
            Move.from move

        to =
            Move.to move

        promotion =
            Move.promotion move
    in
        (if Square.file from /= Square.file to then
            File.toString (Square.file from) ++ "x"
         else
            ""
        )
            ++ Square.toString to
            ++ (case promotion of
                    Nothing ->
                        ""

                    Just promotion_ ->
                        "=" ++ String.toUpper (PieceType.toString promotion_)
               )


disambiguation : Piece -> Move -> Position -> String
disambiguation piece move position =
    let
        moves =
            Position.movesTo (Piece.kind piece) (Move.to move) position
    in
        if List.length moves <= 1 then
            ""
        else if List.all (differentFileFrom move) moves then
            move |> Move.from |> Square.file |> File.toString
        else if List.all (differentRankFrom move) moves then
            move |> Move.from |> Square.rank |> Rank.toString
        else
            move |> Move.from |> Square.toString


differentFileFrom : Move -> Move -> Bool
differentFileFrom m1 m2 =
    (m1 == m2)
        || ((m1 |> Move.from |> Square.file) /= (m2 |> Move.from |> Square.file))


differentRankFrom : Move -> Move -> Bool
differentRankFrom m1 m2 =
    (m1 == m2)
        || ((m1 |> Move.from |> Square.rank) /= (m2 |> Move.from |> Square.rank))


isImportantCharacter : Char -> Bool
isImportantCharacter ch =
    ch /= 'x' && ch /= '=' && ch /= '+' && ch /= '#'


isPieceCharacter : String -> Bool
isPieceCharacter ch =
    ch == "N" || ch == "B" || ch == "R" || ch == "Q" || ch == "K"


kingsideCastlingFromSan : Position -> Maybe Move
kingsideCastlingFromSan position =
    position
        |> Position.moves
        |> List.filter Move.isKingsideCastle
        |> List.head


queensideCastlingFromSan : Position -> Maybe Move
queensideCastlingFromSan position =
    position
        |> Position.moves
        |> List.filter Move.isQueensideCastle
        |> List.head


pieceMoveFromSan : String -> Position -> Maybe Move
pieceMoveFromSan str position =
    let
        kind =
            PieceType.fromString str

        to =
            Square.fromString (String.right 2 str)

        disambig =
            str |> String.dropLeft 1 |> String.dropRight 2
    in
        case kind of
            Nothing ->
                Nothing

            Just kind_ ->
                case to of
                    Nothing ->
                        Nothing

                    Just to_ ->
                        findPieceMove kind_ to_ disambig position


findPieceMove : PieceType -> Square -> String -> Position -> Maybe Move
findPieceMove kind to disambig position =
    let
        dis =
            String.toList disambig

        files =
            List.filterMap File.fromChar dis

        ranks =
            List.filterMap Rank.fromChar dis
    in
        if List.length files > 1 || List.length ranks > 1 then
            Nothing
        else
            let
                file =
                    List.head files

                rank =
                    List.head ranks

                candidates =
                    Position.movesTo kind to position
                        |> List.filter (match kind to file rank)
            in
                if List.length candidates == 1 then
                    List.head candidates
                else
                    Nothing


match :
    PieceType
    -> Square
    -> Maybe SquareFile
    -> Maybe SquareRank
    -> Move
    -> Bool
match kind to fromFile fromRank move =
    case fromFile of
        Nothing ->
            case fromRank of
                Nothing ->
                    True

                Just rank ->
                    rank == Square.rank (Move.from move)

        Just file ->
            case fromRank of
                Nothing ->
                    file == Square.file (Move.from move)

                Just rank ->
                    (file == Square.file (Move.from move))
                        && (rank == Square.rank (Move.to move))


pawnMoveFromSan : String -> Position -> Maybe Move
pawnMoveFromSan string position =
    let
        promotion =
            PieceType.fromString (String.right 1 string)

        str =
            if promotion == Nothing then
                string
            else
                String.dropRight 1 string

        to =
            Square.fromString (String.right 2 str)

        file =
            String.dropRight 2 str
                |> String.toList
                |> List.filterMap File.fromChar
                |> List.head
    in
        case to of
            Nothing ->
                Nothing

            Just to_ ->
                findPawnMove to_ promotion file position


findPawnMove :
    Square
    -> Maybe PieceType
    -> Maybe SquareFile
    -> Position
    -> Maybe Move
findPawnMove to promotion fromFile position =
    let
        candidates =
            Position.movesTo PieceType.pawn to position
                |> List.filter (pawnMatch promotion fromFile)
    in
        if List.length candidates == 1 then
            List.head candidates
        else
            Nothing


pawnMatch : Maybe PieceType -> Maybe SquareFile -> Move -> Bool
pawnMatch promotion fromFile move =
    case promotion of
        Nothing ->
            case fromFile of
                Nothing ->
                    True

                Just file ->
                    (move |> Move.from |> Square.file) == file

        Just promotion_ ->
            case fromFile of
                Nothing ->
                    (move |> Move.promotion) == Just promotion_

                Just file ->
                    ((move |> Move.promotion) == Just promotion_)
                        && ((move |> Move.from |> Square.file) == file)


findMove : Square -> Square -> Maybe PieceType -> Position -> Maybe Move
findMove from to promotion position =
    Position.movesFrom from position
        |> List.filter (\m -> Move.to m == to && Move.promotion m == promotion)
        |> List.head
