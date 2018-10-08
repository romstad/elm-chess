module Internal.Pgn exposing (..)

import Char
import Internal.Game as Game exposing (Game, GameResult(..), TagPair)
import Internal.Move as Move exposing (Move)
import Internal.Notation as Notation
import Parser exposing (..)
import Set


type alias PgnGame =
    { headers : List TagPair
    , moveText : MoveText
    }


type alias TagPair =
    ( String, String )


tagPair a b =
    ( a, b )


type alias MoveText =
    List MoveTextItem


type MoveTextItem
    = Move String
    | MoveNumber
    | Variation MoveText
    | Comment String
    | Nag Int
    | Termination GameResult


pgn : Parser PgnGame
pgn =
    succeed PgnGame
        |= headers
        |= moveText


gameFromString : String -> Maybe Game
gameFromString pgnString =
    run pgn pgnString |> Result.toMaybe |> Maybe.andThen gameFromPgnGame_


gameToString : Game -> String
gameToString game =
    headersToString_ game
        ++ "\n"
        ++ movesToString_ game
        ++ " "
        ++ resultToString_ game



-- Headers


headers : Parser (List TagPair)
headers =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = tagPair_
        , trailing = Optional
        }



-- Moves


moveText : Parser MoveText
moveText =
    loop []
        (\state ->
            oneOf
                [ succeed (\item -> Loop (state ++ [ item ]))
                    |= moveTextItem
                , succeed (Done state)
                    |. end
                , succeed (Loop state)
                    |. spaces
                ]
        )


moveTextItem : Parser MoveTextItem
moveTextItem =
    oneOf
        [ map Comment comment
        , map Termination termination
        , map (\_ -> MoveNumber) moveNumber
        , map Nag nag
        , variation
        , map Move move
        ]


variation : Parser MoveTextItem
variation =
    sequence
        { start = "("
        , separator = ""
        , end = ")"
        , spaces = spaces
        , item = lazy (\_ -> moveTextItem)
        , trailing = Optional
        }
        |> andThen (Variation >> succeed)


moveNumber : Parser Int
moveNumber =
    succeed identity
        |= digits_
        |. symbol "."


move : Parser String
move =
    nonwhitespaceNonparen_ |> andThen disallowBlank_


comment : Parser String
comment =
    succeed identity
        |. symbol "{"
        |= anyCharBut_ '}'
        |. symbol "}"


nag : Parser Int
nag =
    succeed identity
        |. symbol "$"
        |= int


termination : Parser GameResult
termination =
    oneOf
        [ succeed WhiteWins |. keyword "1-0"
        , succeed BlackWins |. keyword "0-1"
        , succeed Draw |. keyword "1/2-1/2"
        , succeed UnknownResult |. symbol "*"
        ]



-- Example


examplePgn : String
examplePgn =
    """[Event "F/S Return Match"]
[Site "Belgrade, Serbia JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2"""



-- Helpers


anyCharBut_ char =
    getChompedString <|
        succeed identity
            |. chompWhile ((/=) char)


checkMoveNumber_ : String -> Parser Int
checkMoveNumber_ number =
    case String.toInt number of
        Just int ->
            succeed int

        Nothing ->
            problem "a move number should be an integer"


digits_ : Parser Int
digits_ =
    getChompedString (chompWhile Char.isDigit)
        |> andThen checkMoveNumber_


disallowBlank_ : String -> Parser String
disallowBlank_ string =
    if string |> String.trim |> String.isEmpty then
        problem "blank"
    else
        succeed string


gameFromPgnGame_ : PgnGame -> Maybe Game
gameFromPgnGame_ g =
    Game.empty
        |> (\game ->
                { game | tags = g.headers, result = result_ g }
                    |> Game.addSanMoveSequence
                        (List.filterMap
                            (\x ->
                                case x of
                                    Move m ->
                                        Just m

                                    _ ->
                                        Nothing
                            )
                            g.moveText
                        )
                    |> Maybe.map Game.toBeginning
           )


headerToString_ : TagPair -> String
headerToString_ ( name, value ) =
    "[" ++ name ++ " " ++ "\"" ++ value ++ "\"" ++ "]" ++ "\n"


headersToString_ : Game -> String
headersToString_ game =
    List.foldl
        (\t result -> result ++ headerToString_ t)
        ""
        game.tags


isNonwhitespaceChar_ char =
    char |> String.fromChar |> String.trim |> String.isEmpty |> not


movesToString_ : Game -> String
movesToString_ game =
    Notation.variationToSan
        (Game.moves game)
        (game |> Game.toBeginning |> Game.position)


nonspaces_ : Parser String
nonspaces_ =
    anyCharBut_ ' '


nonwhitespace_ =
    getChompedString <|
        succeed identity
            |. chompWhile (\char -> char |> String.fromChar |> String.trim |> String.isEmpty |> not)


nonwhitespaceNonparen_ =
    getChompedString <|
        succeed identity
            |. chompWhile (\char -> isNonwhitespaceChar_ char && char /= ')' && char /= '(')


result_ : PgnGame -> GameResult
result_ game =
    List.foldl
        (\x r ->
            case x of
                Termination t ->
                    t

                _ ->
                    r
        )
        UnknownResult
        game.moveText


resultToString_ : Game -> String
resultToString_ game =
    case game.result of
        UnknownResult ->
            "*"

        WhiteWins ->
            "1-0"

        BlackWins ->
            "0-1"

        Draw ->
            "1/2-1/2"


tagPair_ : Parser TagPair
tagPair_ =
    succeed tagPair
        |. symbol "["
        |. spaces
        |= nonspaces_
        |. spaces
        |. symbol "\""
        |= anyCharBut_ '"'
        |. symbol "\""
        |. spaces
        |. symbol "]"
