module PgnTest exposing (suite)

import Test exposing (..)
import ElmTestBDDStyle exposing (..)
import Internal.Game exposing (Game, GameResult(..), TagPair)
import Expect exposing (..)
import Internal.Pgn exposing (..)
import Parser


suite : Test
suite =
    describe "PGN parsing"
        [ describe "pgn parsing a whole game"
            [ it "parses Spassky vs Fischer round 29" <|
                expect (Parser.run pgn examplePgn) to equal (Ok expectedSpasskyFischerGame)
            , it "parses a small example game with variation" <|
                expect (Parser.run pgn pgnWithVariation) to equal (Ok expectedParsedPgnWithVariation)
            ]
        , describe "headers"
            [ it "parses multiple headers" <|
                expect (Parser.run headers headersString)
                    to
                    equal
                    (Ok
                        [ ( "Event", "F/S Return Match" )
                        , ( "Site", "Belgrade, Serbia JUG" )
                        , ( "Date", "1992.11.04" )
                        , ( "Round", "29" )
                        , ( "White", "Fischer, Robert J." )
                        , ( "Black", "Spassky, Boris V." )
                        , ( "Result", "1/2-1/2" )
                        ]
                    )
            ]
        , describe "moves"
            [ it "parses an integer, discarding the dot and space thereafter" <|
                expect (Parser.run moveNumber "1. ") to equal (Ok 1)
            , it "parses a variation" <|
                expect (Parser.run variation variationWithNewline) to equal (Ok expectedParsedVariationWithNewline)
            , it "parses a comment without the {}" <|
                expect (Parser.run comment "{Jo momma can't read}") to equal (Ok "Jo momma can't read")
            , it "parses a move" <|
                expect (Parser.run move "e4") to equal (Ok "e4")
            , test "does not parse a blank move" <|
                (\_ -> Expect.err (Parser.run move ""))
            , describe "termination"
                [ it "parses a draw" <|
                    expect (Parser.run termination "1/2-1/2") to equal (Ok Draw)
                , it "parses a win with the white pieces" <|
                    expect (Parser.run termination "1-0") to equal (Ok WhiteWins)
                , it "parses a win with the black pieces" <|
                    expect (Parser.run termination "0-1") to equal (Ok BlackWins)
                , it "parses an unknown result" <|
                    expect (Parser.run termination "*") to equal (Ok UnknownResult)
                ]
            , describe "moveTextItem"
                [ it "parses a move text item as a move number" <|
                    expect (Parser.run moveTextItem "1. e4 e5 ") to equal (Ok MoveNumber)
                , it "parses e4 as a move" <|
                    expect (Parser.run moveTextItem "e4 e5") to equal (Ok <| Move "e4")
                , it "parses a NAG" <|
                    expect (Parser.run moveTextItem "$0 4. g6") to equal (Ok <| Nag 0)
                , it "parses a termination" <|
                    expect (Parser.run moveTextItem "1/2-1/2") to equal (Ok <| Termination Draw)
                ]
            , it "parses one move number" <|
                expect (Parser.run moveText "1. ") to equal (Ok [ MoveNumber ])
            , it "parses a move number and a move" <|
                expect (Parser.run moveText "1. e4 ") to equal (Ok [ MoveNumber, Move "e4" ])
            , it "parses a list of moves" <|
                expect (Parser.run moveText "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}")
                    to
                    equal
                    (Ok
                        [ MoveNumber
                        , Move "e4"
                        , Move "e5"
                        , MoveNumber
                        , Move "Nf3"
                        , Move "Nc6"
                        , MoveNumber
                        , Move "Bb5"
                        , Move "a6"
                        , Comment "This opening is called the Ruy Lopez."
                        ]
                    )
            ]
        ]


headersString =
    """
[Event "F/S Return Match"]
[Site "Belgrade, Serbia JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]


"""


variationWithNewline =
    """(2... Nc6 {is the usual reply here, and it generally
leads to the Ruy Lopez opening with} 3. Bb5)
"""


expectedParsedVariationWithNewline =
    Variation
        [ MoveNumber
        , Move ".."
        , Move "Nc6"
        , Comment "is the usual reply here, and it generally\nleads to the Ruy Lopez opening with"
        , MoveNumber
        , Move "Bb5"
        ]


pgnWithVariation =
    """
[Game "Unknown"]

1. e4 e5 2. Nf3 Nf6
(2... Nc6 {is the usual reply here, and it generally
leads to the Ruy Lopez opening with} 3. Bb5)
3. Nxe5 d6 4. Nf3 Nxe4 5. d4 d5"""


expectedParsedPgnWithVariation =
    { headers = [ ( "Game", "Unknown" ) ]
    , moveText =
        [ MoveNumber
        , Move "e4"
        , Move "e5"
        , MoveNumber
        , Move "Nf3"
        , Move "Nf6"
        , Variation [ MoveNumber, Move "..", Move "Nc6", Comment "is the usual reply here, and it generally\nleads to the Ruy Lopez opening with", MoveNumber, Move "Bb5" ]
        , MoveNumber
        , Move "Nxe5"
        , Move "d6"
        , MoveNumber
        , Move "Nf3"
        , Move "Nxe4"
        , MoveNumber
        , Move "d4"
        , Move "d5"
        ]
    }


expectedSpasskyFischerGame =
    { headers =
        [ ( "Event", "F/S Return Match" )
        , ( "Site", "Belgrade, Serbia JUG" )
        , ( "Date", "1992.11.04" )
        , ( "Round", "29" )
        , ( "White", "Fischer, Robert J." )
        , ( "Black", "Spassky, Boris V." )
        , ( "Result", "1/2-1/2" )
        ]
    , moveText =
        [ MoveNumber
        , Move "e4"
        , Move "e5"
        , MoveNumber
        , Move "Nf3"
        , Move "Nc6"
        , MoveNumber
        , Move "Bb5"
        , Move "a6"
        , Comment "This opening is called the Ruy Lopez."
        , MoveNumber
        , Move "Ba4"
        , Move "Nf6"
        , MoveNumber
        , Move "O-O"
        , Move "Be7"
        , MoveNumber
        , Move "Re1"
        , Move "b5"
        , MoveNumber
        , Move "Bb3"
        , Move "d6"
        , MoveNumber
        , Move "c3"
        , Move "O-O"
        , MoveNumber
        , Move "h3"
        , Move "Nb8"
        , MoveNumber
        , Move "d4"
        , Move "Nbd7"
        , MoveNumber
        , Move "c4"
        , Move "c6"
        , MoveNumber
        , Move "cxb5"
        , Move "axb5"
        , MoveNumber
        , Move "Nc3"
        , Move "Bb7"
        , MoveNumber
        , Move "Bg5"
        , Move "b4"
        , MoveNumber
        , Move "Nb1"
        , Move "h6"
        , MoveNumber
        , Move "Bh4"
        , Move "c5"
        , MoveNumber
        , Move "dxe5"
        , Move "Nxe4"
        , MoveNumber
        , Move "Bxe7"
        , Move "Qxe7"
        , MoveNumber
        , Move "exd6"
        , Move "Qf6"
        , MoveNumber
        , Move "Nbd2"
        , Move "Nxd6"
        , MoveNumber
        , Move "Nc4"
        , Move "Nxc4"
        , MoveNumber
        , Move "Bxc4"
        , Move "Nb6"
        , MoveNumber
        , Move "Ne5"
        , Move "Rae8"
        , MoveNumber
        , Move "Bxf7+"
        , Move "Rxf7"
        , MoveNumber
        , Move "Nxf7"
        , Move "Rxe1+"
        , MoveNumber
        , Move "Qxe1"
        , Move "Kxf7"
        , MoveNumber
        , Move "Qe3"
        , Move "Qg5"
        , MoveNumber
        , Move "Qxg5"
        , Move "hxg5"
        , MoveNumber
        , Move "b3"
        , Move "Ke6"
        , MoveNumber
        , Move "a3"
        , Move "Kd6"
        , MoveNumber
        , Move "axb4"
        , Move "cxb4"
        , MoveNumber
        , Move "Ra5"
        , Move "Nd5"
        , MoveNumber
        , Move "f3"
        , Move "Bc8"
        , MoveNumber
        , Move "Kf2"
        , Move "Bf5"
        , MoveNumber
        , Move "Ra7"
        , Move "g6"
        , MoveNumber
        , Move "Ra6+"
        , Move "Kc5"
        , MoveNumber
        , Move "Ke1"
        , Move "Nf4"
        , MoveNumber
        , Move "g3"
        , Move "Nxh3"
        , MoveNumber
        , Move "Kd2"
        , Move "Kb5"
        , MoveNumber
        , Move "Rd6"
        , Move "Kc5"
        , MoveNumber
        , Move "Ra6"
        , Move "Nf2"
        , MoveNumber
        , Move "g4"
        , Move "Bd3"
        , MoveNumber
        , Move "Re6"
        , Termination Draw
        ]
    }
