module Internal.AnnotatedGame exposing
    ( Game
    , addMove
    , addSanMove
    , addSanMoveSequence
    , back
    , empty
    , forward
    , goToMove
    , isAtBeginning
    , isAtEnd
    , moves
    , nextMove
    , position
    , previousMove
    , tagValue
    , toBeginning
    , toEnd
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
import Internal.Position as Position exposing (Position)
import Internal.Util exposing (failableFoldl)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias GameNode =
    { position : Position
    , comment : Maybe String
    , precomment : Maybe String
    , nag : Maybe Int
    , id : Int
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


{-| The current node in the game tree
-}
currentNode : Game -> GameNode
currentNode game =
    Zipper.label game.focus


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
            }

        z =
            if isAtEnd game then
                Zipper.replaceTree
                    (Tree.tree (Zipper.label game.focus) [ Tree.singleton node ])
                    game.focus

            else
                Zipper.append (Tree.singleton node) game.focus
    in
    { game
        | tree = Zipper.toTree z
        , focus = Maybe.withDefault z <| Zipper.firstChild z
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
move in short algebraic notation, returns Nothing.
-}
addSanMoveSequence : List String -> Game -> Maybe Game
addSanMoveSequence sanMoves game =
    failableFoldl addSanMove game sanMoves


fromPgnGame : PgnGame -> Game
fromPgnGame pgnGame =
    List.foldl
        addMoveTextItem
        { empty | tags = pgnGame.headers }
        pgnGame.moveText


addMoveTextItem : MoveTextItem -> Game -> Game
addMoveTextItem mti game =
    case mti of
        Move move ->
            Maybe.withDefault game (addSanMove move game)

        MoveNumber ->
            game

        Variation var ->
            addVariation var game

        Comment cmt ->
            if isAtBeginning game || not (isAtEnd game) then
                game

            else
                addComment cmt game

        Nag nag ->
            addNag nag game

        Termination t ->
            { game | result = t }


addPreComment : String -> Game -> Game
addPreComment cmt game =
    { game
        | focus =
            Zipper.mapLabel (\n -> { n | precomment = Just cmt }) game.focus
    }


addComment : String -> Game -> Game
addComment cmt game =
    { game
        | focus =
            Zipper.mapLabel (\n -> { n | comment = Just cmt }) game.focus
    }


addNag : Int -> Game -> Game
addNag nag game =
    { game
        | focus =
            Zipper.mapLabel (\n -> { n | nag = Just nag }) game.focus
    }


addVariation : MoveText -> Game -> Game
addVariation variation game =
    game
        |> back
        |> (\g -> List.foldl addMoveTextItem g variation)
        |> toBeginningOfVariation
        |> forward
