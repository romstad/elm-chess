module Internal.Game exposing
    ( Game
    , GameResult(..)
    , TagPair
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
import Internal.Move as Move exposing (Move)
import Internal.Notation exposing (fromSan)
import Internal.Position as Position exposing (Position)
import Internal.Util exposing (failableFoldl)



{- Later on, games should probably be some kind of tree like data type, like in
   clj-chess. For now, we just use a plain array of positions along with an
   index to the position at the current move.
-}


type alias Game =
    { result : GameResult
    , tags : List TagPair
    , positions : Array Position
    , currentPosition : Position
    , currentMoveIndex : Int
    }


type alias TagPair =
    ( String, String )


type GameResult
    = WhiteWins
    | BlackWins
    | Draw
    | UnknownResult


{-| A new game starting from the standard opening position.
-}
empty : Game
empty =
    { result = UnknownResult
    , tags = []
    , positions = Array.fromList [ Position.initial ]
    , currentPosition = Position.initial
    , currentMoveIndex = 0
    }


{-| Get the value of a game tag.
-}
tagValue : String -> Game -> Maybe String
tagValue tagName game =
    game.tags
        |> List.filter (\t -> Tuple.first t == tagName)
        |> List.head
        |> Maybe.map Tuple.second


{-| The position at the current move of the game.
-}
position : Game -> Position
position game =
    game.currentPosition


{-| List of all moves in the game.
-}
moves : Game -> List Move
moves game =
    let
        movesInternal result pos =
            case Position.lastMove pos of
                Nothing ->
                    result

                Just m ->
                    case Position.parent pos of
                        Nothing ->
                            result

                        Just p ->
                            movesInternal (m :: result) p
    in
    game
        |> toEnd
        |> position
        |> movesInternal []


{-| The previous move in the game, i.e. the move that resulted in the
current game position. Returns `Nothing` if we're at the beginning of the
game.
-}
previousMove : Game -> Maybe Move
previousMove game =
    game |> position |> Position.lastMove


{-| The next move in the game from the current position. Returns `Nothing` if
we're at the end of the game.
-}
nextMove : Game -> Maybe Move
nextMove game =
    if isAtEnd game then
        Nothing

    else
        game |> forward |> previousMove


{-| Jump to the given game ply number.
-}
goToMove : Int -> Game -> Game
goToMove moveIndex game =
    if moveIndex < 0 || moveIndex >= Array.length game.positions then
        game

    else
        { game
            | currentMoveIndex = moveIndex
            , currentPosition =
                Maybe.withDefault
                    Position.initial
                    (Array.get moveIndex game.positions)
        }


{-| Step one move forward, unless we are already at the end of the game.
If we are already at the end, do nothing.
-}
forward : Game -> Game
forward game =
    goToMove (game.currentMoveIndex + 1) game


{-| Step one move backward, unless we are already at the beginning of the game.
If we are already at the beginning, do nothing.
-}
back : Game -> Game
back game =
    goToMove (game.currentMoveIndex - 1) game


{-| Go to the beginning of the game.
-}
toBeginning : Game -> Game
toBeginning game =
    goToMove 0 game


{-| Go to the end of the game.
-}
toEnd : Game -> Game
toEnd game =
    goToMove (Array.length game.positions - 1) game


{-| Are we at the beginning of the game?
-}
isAtBeginning : Game -> Bool
isAtBeginning game =
    game.currentMoveIndex == 0


{-| Are we at the end of the game?
-}
isAtEnd : Game -> Bool
isAtEnd game =
    game.currentMoveIndex == Array.length game.positions - 1


{-| Add a move to the game at the current move index. Any previous game
continuation will be overwritten.
-}
addMove : Move -> Game -> Game
addMove move game =
    let
        pos =
            Position.doMove move (position game)
    in
    { game
        | positions =
            Array.slice 0 (1 + game.currentMoveIndex) game.positions
                |> Array.push pos
        , currentMoveIndex = game.currentMoveIndex + 1
        , currentPosition = pos
    }


{-| Tries to add a move in short algebraic notation at the current move index.
Any previous game continuation will be overwritten. If the input string is not
a legal, unambiguous move in short algebraic notation from the current game
position, returns nil.
-}
addSanMove : String -> Game -> Maybe Game
addSanMove sanMove game =
    Maybe.map (\m -> addMove m game) <|
        fromSan sanMove game.currentPosition


{-| Tries to add a sequence of moves in short algebraic notation at the current
move index. Any previous game continuation will be overwritten. If one of the
strings in the input list is not a legal, unambiguous move in short algebraic
notation, returns nil.
-}
addSanMoveSequence : List String -> Game -> Maybe Game
addSanMoveSequence sanMoves game =
    failableFoldl addSanMove game sanMoves
