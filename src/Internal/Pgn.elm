module Internal.Pgn exposing (gameFromString, gameToString)

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


type alias MoveText =
    List MoveTextItem


type MoveTextItem
    = Move String
    | MoveNumber
    | Variation MoveText
    | Comment String
    | Nag Int
    | Termination GameResult


gameFromString : String -> Maybe Game
gameFromString pgnString =
    fromString pgnString
        |> Maybe.andThen gameFromPgnGame


gameToString : Game -> String
gameToString game =
    headersToString game
        ++ "\n"
        ++ movesToString game
        ++ " "
        ++ resultToString game


gameFromPgnGame : PgnGame -> Maybe Game
gameFromPgnGame g =
    Game.empty
        |> (\game ->
                { game | tags = g.headers, result = result g }
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


tagValue : String -> PgnGame -> Maybe String
tagValue tagName game =
    game.headers
        |> List.filter (\t -> Tuple.first t == tagName)
        |> List.head
        |> Maybe.map Tuple.second


result : PgnGame -> GameResult
result game =
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


fromString : String -> Maybe PgnGame
fromString pgnString =
    Parser.run pgn pgnString |> Result.toMaybe


pgn : Parser PgnGame
pgn =
    Parser.succeed PgnGame
        |= headers
        |= moveText


headers : Parser (List TagPair)
headers =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = tagPair
        , trailing = Parser.Optional
        }


tagPair : Parser TagPair
tagPair =
    succeed (\a b -> ( a, b ))
        |. symbol "["
        |. spaces
        |= tag1
        |. spaces
        |. symbol "\""
        |= tag2
        |. symbol "\""
        |. spaces
        |. symbol "]"
        |. chompUntilEndOr "\n"


tag1 : Parser String
tag1 =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c /= ' ')


tag2 : Parser String
tag2 =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c /= '"')


moveText : Parser MoveText
moveText =
    Parser.repeat Parser.zeroOrMore <|
        Parser.succeed identity
            |. whitespaceOrPeriod
            |= moveTextItem


moveTextItem : Parser MoveTextItem
moveTextItem =
    Parser.oneOf
        [ Parser.map Termination termination
        , Parser.map (\_ -> MoveNumber) moveNumber
        , Parser.map Move move
        , Parser.map Comment comment
        , Parser.map Nag nag
        , Parser.succeed Variation
            |. Parser.symbol "("
            |= Parser.lazy (\_ -> moveText)
            |. Parser.symbol ")"
        ]


termination : Parser GameResult
termination =
    Parser.oneOf
        [ Parser.succeed WhiteWins |. Parser.keyword "1-0"
        , Parser.succeed BlackWins |. Parser.keyword "0-1"
        , Parser.succeed Draw |. Parser.keyword "1/2-1/2"
        , Parser.succeed UnknownResult |. Parser.symbol "*"
        ]


moveNumber : Parser ()
moveNumber =
    Parser.succeed ()
        |. Parser.ignore Parser.oneOrMore Char.isDigit


move : Parser String
move =
    symbol


comment : Parser String
comment =
    Parser.succeed identity
        |. Parser.symbol "{"
        |= Parser.keep Parser.zeroOrMore ((/=) '}')
        |. Parser.symbol "}"


nag : Parser Int
nag =
    Parser.succeed identity
        |. Parser.symbol "$"
        |= Parser.int


string : Parser String
string =
    Parser.succeed (List.foldr (++) "")
        |. Parser.symbol "\""
        |= Parser.repeat
            Parser.zeroOrMore
            (Parser.oneOf
                [ escapedChar
                , Parser.keep Parser.oneOrMore (\c -> c /= '"' && c /= '\\')
                ]
            )
        |. Parser.symbol "\""


escapedChar : Parser String
escapedChar =
    Parser.succeed identity
        |. Parser.symbol "\\"
        |= Parser.oneOf
            [ Parser.map (\_ -> "\"") (Parser.symbol "\"")
            , Parser.map (\_ -> "\\") (Parser.symbol "\\")
            ]


symbol : Parser String
symbol =
    Parser.succeed (++)
        |= Parser.keep (Parser.Exactly 1) isSymbolStart
        |= Parser.keep (Parser.AtLeast 0) isSymbolContinuation


isSymbolStart : Char -> Bool
isSymbolStart char =
    Char.isUpper char
        || Char.isLower char
        || Char.isDigit char


isSymbolContinuation : Char -> Bool
isSymbolContinuation char =
    isSymbolStart char
        || Set.member
            char
            (Set.fromList [ '_', '+', '#', '=', ':', '-', '/' ])


whitespace =
    Parser.spaces



--whitespace : Parser ()
--whitespace =
--    Parser.ignore Parser.zeroOrMore <|
--        \c -> c == ' ' || c == '\n'


whitespaceOrPeriod =
    Parser.chompIf (\c -> c == ' ' || c == '\n' || c == '.')



--whitespaceOrPeriod : Parser ()
--whitespaceOrPeriod =
--    Parser.ignore Parser.zeroOrMore <|
--        \c -> c == ' ' || c == '\n' || c == '.'


headersToString : Game -> String
headersToString game =
    List.foldl
        (\t result_ -> result_ ++ headerToString t)
        ""
        game.tags


headerToString : TagPair -> String
headerToString ( name, value ) =
    "[" ++ name ++ " " ++ "\"" ++ value ++ "\"" ++ "]" ++ "\n"


movesToString : Game -> String
movesToString game =
    Notation.variationToSan
        (Game.moves game)
        (game |> Game.toBeginning |> Game.position)


resultToString : Game -> String
resultToString game =
    case game.result of
        UnknownResult ->
            "*"

        WhiteWins ->
            "1-0"

        BlackWins ->
            "0-1"

        Draw ->
            "1/2-1/2"


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


moves : Game -> List Move
moves game =
    Game.moves game
