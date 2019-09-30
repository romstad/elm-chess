module Internal.AnnotatedGame exposing
    ( Game
    , addMove
    , addSanMove
    , addSanMoveSequence
    , back
    , buildMoveText
    , children
    , continuations
    , empty
    , examplePgn
    , forward
    , fromPgn
    , fromPgnString
    , goToMove
    , isAtBeginning
    , isAtBeginningOfVariation
    , isAtEnd
    , moves
    , nextMove
    , position
    , previousMove
    , tagValue
    , toBeginning
    , toBeginningOfVariation
    , toEnd
    , toEndOfVariation
    , toPgn
    )

import Array exposing (Array)
import Internal.AnnotatedPgn as Pgn
    exposing
        ( GameResult(..)
        , MoveText
        , MoveTextItem(..)
        , PgnGame
        , TagPair
        )
import Internal.Move as Move exposing (Move, Variation)
import Internal.Notation exposing (fromSan, toSan)
import Internal.PieceColor as PieceColor exposing (PieceColor)
import Internal.Position as Position exposing (Position)
import Internal.Util exposing (failableFoldl)
import Parser
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias GameNode =
    { position : Position
    , comment : Maybe String
    , precomment : Maybe String
    , nag : Maybe Int
    , id : Int
    , ply : Int
    }


type alias Game =
    { result : GameResult
    , tags : List TagPair
    , tree : Tree GameNode
    , focus : Zipper GameNode
    , nodeCount : Int
    }


empty : Game
empty =
    let
        tree =
            Tree.singleton
                { position = Position.initial
                , comment = Nothing
                , precomment = Nothing
                , nag = Nothing
                , id = 0
                , ply = 0
                }
    in
    { result = UnknownResult
    , tags = []
    , tree = tree
    , focus = Zipper.fromTree tree
    , nodeCount = 1
    }


{-| Get the value of a game tag.
-}
tagValue : String -> Game -> Maybe String
tagValue tagName game =
    game.tags
        |> List.filter (\t -> Tuple.first t == tagName)
        |> List.head
        |> Maybe.map Tuple.second


lastMove : GameNode -> Move
lastMove node =
    Maybe.withDefault (Move.Move 0) (Position.lastMove node.position)


{-| The current node in the game tree
-}
currentNode : Game -> GameNode
currentNode game =
    Zipper.label game.focus


{-| Children of the current node in the game tree
-}
children : Game -> List GameNode
children game =
    List.map Tree.label (Zipper.children game.focus)


{-| Continuations from the current node in the game tree
-}
continuations : Game -> List Move
continuations game =
    List.map
        (\n ->
            Maybe.withDefault
                (Move.Move 0)
                (Position.lastMove n.position)
        )
        (children game)


{-| The position at the current move of the game.
-}
position : Game -> Position
position game =
    (currentNode game).position


{-| List of all moves in the game.
-}
moves : Game -> List Move
moves game =
    let
        zipper =
            Zipper.fromTree game.tree

        movesInternal z =
            case Zipper.firstChild z of
                Nothing ->
                    []

                Just zz ->
                    case Position.lastMove (Zipper.label zz).position of
                        Nothing ->
                            []

                        Just m ->
                            m :: movesInternal zz
    in
    movesInternal (Zipper.fromTree game.tree)


{-| The previous move in the game, i.e. the move that resulted in the
current game position. Returns `Nothing` if we're at the beginning of the
game.
-}
previousMove : Game -> Maybe Move
previousMove game =
    Position.lastMove (position game)


{-| The next move in the game from the current position. Returns `Nothing` if
we're at the end of the game.
-}
nextMove : Game -> Maybe Move
nextMove game =
    Zipper.firstChild game.focus
        |> Maybe.andThen (\z -> Position.lastMove (Zipper.label z).position)


{-| Jump to the given game ply number.
-}
goToMove : Int -> Game -> Game
goToMove moveIndex game =
    if moveIndex <= 0 then
        { game | focus = Zipper.fromTree game.tree }

    else
        let
            descendant zipper generations =
                if generations == 0 then
                    zipper

                else
                    case Zipper.firstChild zipper of
                        Nothing ->
                            zipper

                        Just z ->
                            descendant z (generations - 1)
        in
        { game | focus = descendant (Zipper.fromTree game.tree) moveIndex }


{-| Jump to the node with the given node ID, if it exists. If not, return the
game unchanged. Perhaps this one should return a `Maybe Game` instead?
-}
goToNode : Int -> Game -> Game
goToNode nodeId game =
    { game
        | focus =
            Maybe.withDefault game.focus <|
                Zipper.findFromRoot (\n -> n.id == nodeId) game.focus
    }


{-| Are we at the beginning of the game?
-}
isAtBeginning : Game -> Bool
isAtBeginning game =
    Zipper.label game.focus == Tree.label game.tree


{-| Are we at the end of the game?
-}
isAtEnd : Game -> Bool
isAtEnd game =
    Zipper.firstChild game.focus == Nothing


{-| Step one move forward, unless we are already at the end of the game.
If we are already at the end, do nothing.
-}
forward : Game -> Game
forward game =
    { game
        | focus = Maybe.withDefault game.focus (Zipper.firstChild game.focus)
    }


{-| Step one move backward, unless we are already at the beginning of the game.
If we are already at the beginning, do nothing.
-}
back : Game -> Game
back game =
    { game
        | focus = Maybe.withDefault game.focus (Zipper.parent game.focus)
    }


{-| Go to the beginning of the game.
-}
toBeginning : Game -> Game
toBeginning game =
    { game | focus = Zipper.root game.focus }


{-| Go to the end of the game.
-}
toEnd : Game -> Game
toEnd game =
    toEndOfVariation <| toBeginning game


isAtBeginningOfVariation : Game -> Bool
isAtBeginningOfVariation game =
    case Zipper.parent game.focus of
        Nothing ->
            True

        Just parent ->
            Zipper.firstChild parent /= Just game.focus


{-| Go to the beginning of the current variation.
-}
toBeginningOfVariation : Game -> Game
toBeginningOfVariation game =
    if isAtBeginningOfVariation game then
        game

    else
        toBeginningOfVariation (back game)


{-| Go to the end of the current variation.
-}
toEndOfVariation : Game -> Game
toEndOfVariation game =
    { game | focus = Zipper.lastDescendant game.focus }


{-| Add a move to the game at the current location. If we're not at a leaf
node, the move is added as an alternative variation.
-}
addMove : Move -> Game -> Game
addMove move game =
    let
        pos =
            Position.doMove move (position game)

        node =
            { position = pos
            , comment = Nothing
            , precomment = Nothing
            , nag = Nothing
            , id = game.nodeCount
            , ply = (Zipper.label game.focus).ply + 1
            }

        z =
            let
                forest =
                    Zipper.children game.focus
            in
            Zipper.replaceTree
                (Tree.tree
                    (Zipper.label game.focus)
                    (forest ++ [ Tree.singleton node ])
                )
                game.focus
    in
    { game
        | tree = Zipper.toTree z
        , focus = Maybe.withDefault z <| Zipper.lastChild z
        , nodeCount = game.nodeCount + 1
    }


{-| Tries to add a move in short algebraic notation at the current move index.
If the input string is not a legal, unambiguous move in short algebraic notation
from the current game position, returns Nothing.
-}
addSanMove : String -> Game -> Maybe Game
addSanMove sanMove game =
    Maybe.map (\m -> addMove m game) <|
        fromSan sanMove (position game)


{-| Tries to add a sequence of moves in short algebraic notation at the current
move index. If one of the strings in the input list is not a legal, unambiguous
move in short algebraic notation, returns `Nothing`.
-}
addSanMoveSequence : List String -> Game -> Maybe Game
addSanMoveSequence sanMoves game =
    failableFoldl addSanMove game sanMoves


fromPgn : PgnGame -> Game
fromPgn pgnGame =
    Tuple.first <|
        List.foldl
            addMoveTextItem
            ( { empty | tags = pgnGame.headers }, Nothing )
            pgnGame.moveText


fromPgnString : String -> Maybe Game
fromPgnString pgnString =
    Maybe.map fromPgn (Pgn.fromString pgnString)


toPgn : Game -> PgnGame
toPgn game =
    { headers = game.tags
    , moveText =
        (if Position.sideToMove (position <| toBeginning game) == PieceColor.black then
            [ MoveNumber "1." ]

         else
            []
        )
            ++ (buildMoveText <|
                    Zipper.fromTree <|
                        game.tree
               )
            ++ [ Termination game.result ]
    }


addMoveTextItem : MoveTextItem -> ( Game, Maybe String ) -> ( Game, Maybe String )
addMoveTextItem mti ( game, preCmt ) =
    case mti of
        Move move ->
            case preCmt of
                Nothing ->
                    ( Maybe.withDefault game (addSanMove move game), Nothing )

                Just pc ->
                    ( addPreComment pc
                        (Maybe.withDefault
                            game
                            (addSanMove move game)
                        )
                    , Nothing
                    )

        MoveNumber _ ->
            ( game, preCmt )

        Variation var ->
            ( addVariation var game, Nothing )

        Comment cmt ->
            if isAtBeginning game || not (isAtEnd game) then
                ( game, Just cmt )

            else
                ( addComment cmt game, Nothing )

        Nag nag ->
            ( addNag nag game, Nothing )

        Termination t ->
            ( { game | result = t }, Nothing )


addPreComment : String -> Game -> Game
addPreComment cmt game =
    { game
        | focus =
            Zipper.mapLabel
                (\n -> { n | precomment = Just cmt })
                game.focus
    }


addComment : String -> Game -> Game
addComment cmt game =
    { game
        | focus =
            Zipper.mapLabel
                (\n -> { n | comment = Just cmt })
                game.focus
    }


addNag : Int -> Game -> Game
addNag nag game =
    { game
        | focus =
            Zipper.mapLabel
                (\n -> { n | nag = Just nag })
                game.focus
    }


addVariation : MoveText -> Game -> Game
addVariation variation game =
    game
        |> back
        |> (\g -> Tuple.first <| List.foldl addMoveTextItem ( g, Nothing ) variation)
        |> toBeginningOfVariation
        |> back
        |> forward


buildMoveText : Zipper GameNode -> MoveText
buildMoveText zip =
    case Zipper.firstChild zip of
        Nothing ->
            []

        Just ch ->
            let
                pos =
                    (Zipper.label zip).position
            in
            moveToMoveText
                pos
                (Zipper.label ch)
                (Position.sideToMove pos == PieceColor.white)
                ++ -- Alternative variations
                   List.map
                    (\t ->
                        Variation <|
                            moveToMoveText pos (Tree.label t) True
                                ++ buildMoveText (Zipper.fromTree t)
                    )
                    (Zipper.siblingsAfterFocus ch)
                ++ -- Game continuation
                   buildMoveText ch


moveToMoveText : Position -> GameNode -> Bool -> MoveText
moveToMoveText pos node moveNum =
    let
        move =
            lastMove node
    in
    -- Pre-comment
    (case node.precomment of
        Nothing ->
            []

        Just cmt ->
            [ Comment cmt ]
    )
        -- Move number
        ++ (if moveNum then
                [ MoveNumber
                    (if Position.sideToMove pos == PieceColor.white then
                        String.fromInt (node.ply // 2 + 1) ++ "."

                     else
                        String.fromInt (node.ply // 2 + 1) ++ "..."
                    )
                ]

            else
                []
           )
        -- Move
        ++ [ Move <| toSan move pos ]
        -- Numeric Annotation Glyph
        ++ (case node.nag of
                Nothing ->
                    []

                Just nag ->
                    [ Nag nag ]
           )
        -- Comment
        ++ (case node.comment of
                Nothing ->
                    []

                Just cmt ->
                    [ Comment cmt ]
           )


examplePgn =
    """[Event "F/S Return Match"]
[Site "Belgrade, Serbia JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]

1. e4 e5 (1... c5 2. Nf3 d6) 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
4. Ba4 ({If } 4. Bxc6  dxc6 { visible? } 5. O-O (5. Nxe5 Qd4) f6 {End of var}) Nf6 {After var} 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 { boring } 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2"""
