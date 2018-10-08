module Game
    exposing
        ( Game
        , empty
        , fromPgn
        , toPgn
        , position
        , moves
        , previousMove
        , nextMove
        , goToMove
        , forward
        , back
        , toBeginning
        , toEnd
        , isAtBeginning
        , isAtEnd
        , addMove
        )

{-| The `Game` type and related functions.

At the moment, a game is little more than a sequence of moves, and a current
position. There is no support for annotation in the form of comments or
variations. Support for tree-like games with recursive variations and
comments are planned for a future version of this library.


# Types

@docs Game


# Constants

@docs empty


# Converting Games to and from PGN

@docs fromPgn, toPgn


# Getting the Current Position

@docs position


# Getting Game Moves

@docs moves, previousMove, nextMove


# Navigating in the Game.

@docs goToMove, forward, back, toBeginning, toEnd, isAtBeginning, isAtEnd


# Adding New Moves

@docs addMove

-}

import Internal.Game as Internal
import Internal.Pgn exposing (gameFromString, gameToString)
import Move exposing (Move)
import Position exposing (Position)


{-| Type representing a chess game.
-}
type Game
    = Game Internal.Game


{-| A new game starting from the standard opening position.
-}
empty : Game
empty =
    Game Internal.empty


{-| Tries to create a `Game` from a PGN string. Returns `Nothing` if PGN
parsing fails, or if there are illegal or ambiguous moves.
-}
fromPgn : String -> Maybe Game
fromPgn pgnString =
    gameFromString pgnString
        |> Maybe.map Game


{-| Converts a game to a string in PGN format.
-}
toPgn : Game -> String
toPgn game =
    case game of
        Game g ->
            gameToString g


{-| The current position in the game.
-}
position : Game -> Position
position game =
    case game of
        Game g ->
            Internal.position g


{-| List of all moves in the game.
-}
moves : Game -> List Move
moves game =
    case game of
        Game g ->
            Internal.moves g


{-| The previous move in the game, i.e. the move that resulted in the
current game position. Returns `Nothing` if we're at the beginning of the
game.
-}
previousMove : Game -> Maybe Move
previousMove game =
    case game of
        Game g ->
            Internal.previousMove g


{-| The next move in the game from the current position. Returns `Nothing` if
we're at the end of the game.
-}
nextMove : Game -> Maybe Move
nextMove game =
    case game of
        Game g ->
            Internal.nextMove g


{-| Jump to the given game ply number. If the ply is less than 0, goes to the
beginning of the game. If the ply is bigger than the length of the game, goes
to the end of the game.
-}
goToMove : Int -> Game -> Game
goToMove moveIndex game =
    case game of
        Game g ->
            Game <| Internal.goToMove moveIndex g


{-| Step one move forward, unless we are already at the end of the game.
If we are already at the end, do nothing.
-}
forward : Game -> Game
forward game =
    case game of
        Game g ->
            Game <| Internal.forward g


{-| Step one move backward, unless we are already at the beginning of the game.
If we are already at the beginning, do nothing.
-}
back : Game -> Game
back game =
    case game of
        Game g ->
            Game <| Internal.back g


{-| Go to the beginning of the game.
-}
toBeginning : Game -> Game
toBeginning game =
    case game of
        Game g ->
            Game <| Internal.toBeginning g


{-| Go to the end of the game.
-}
toEnd : Game -> Game
toEnd game =
    case game of
        Game g ->
            Game <| Internal.toEnd g


{-| Are we at the beginning of the game?
-}
isAtBeginning : Game -> Bool
isAtBeginning game =
    case game of
        Game g ->
            Internal.isAtBeginning g


{-| Are we at the beginning of the game?
-}
isAtEnd : Game -> Bool
isAtEnd game =
    case game of
        Game g ->
            Internal.isAtEnd g


{-| Add a move to the game at the current move index. Any previous game
continuation will be overwritten.
-}
addMove : Move -> Game -> Game
addMove move game =
    case game of
        Game g ->
            Game <| Internal.addMove move g
