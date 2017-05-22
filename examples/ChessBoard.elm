module ChessBoard exposing (..)

import Css
import Game exposing (Game)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Move exposing (Move)
import Piece exposing (Piece)
import PieceColor
import PieceType
import Position exposing (Position)
import Square exposing (Square)
import SquareFile as File
import SquareRank as Rank


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { game : Game
    , boardIsRotated : Bool
    , selectedSquare : Maybe Square -- Square tapped by the user
    , candidateMoves : List Move -- List of legal moves from selected square
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Game.empty
      , boardIsRotated = False
      , selectedSquare = Nothing
      , candidateMoves = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SquarePressed Square
    | DoMove Move
    | RotateBoard
    | Back
    | Forward


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquarePressed sq ->
            squarePressed sq model

        DoMove move ->
            ( { model
                | game = Game.addMove move model.game
                , selectedSquare = Nothing
                , candidateMoves = []
              }
            , Cmd.none
            )

        RotateBoard ->
            ( { model | boardIsRotated = not model.boardIsRotated }
            , Cmd.none
            )

        Back ->
            ( { model | game = Game.back model.game }
            , Cmd.none
            )

        Forward ->
            ( { model | game = Game.forward model.game }
            , Cmd.none
            )


squarePressed : Square -> Model -> ( Model, Cmd Msg )
squarePressed s model =
    let
        moves =
            List.filter (\m -> Move.to m == s) model.candidateMoves
    in
        case List.head moves of
            Just m ->
                update (DoMove m) model

            Nothing ->
                let
                    newMoves =
                        Game.position (model.game) |> Position.movesFrom s
                in
                    ( { model
                        | candidateMoves = newMoves
                        , selectedSquare =
                            if List.length newMoves == 0 then
                                Nothing
                            else
                                Just s
                      }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ board (Game.position model.game) 400.0 model.boardIsRotated
        , div
            []
            [ button [ onClick RotateBoard ] [ text "Rotate" ]
            , button [ onClick Back ] [ text "Back" ]
            , button [ onClick Forward ] [ text "Forward" ]
            ]
        ]


board : Position -> Float -> Bool -> Html Msg
board position size isRotated =
    Html.div
        [ styles
            [ Css.width (Css.px size)
            , Css.height (Css.px size)
            , Css.position Css.relative
            , Css.marginLeft Css.auto
            , Css.marginRight Css.auto
            ]
        ]
        (List.map
            (\s ->
                square
                    (squareToCoordinates s isRotated)
                    (Position.pieceOn s position)
                    (size / 8)
                    (SquarePressed s)
            )
            Square.all
        )


square : ( Int, Int ) -> Maybe Piece -> Float -> Msg -> Html Msg
square ( col, row ) piece sqSize msg =
    Html.div
        [ styles
            [ Css.backgroundColor
                (if (col + row) % 2 == 0 then
                    Css.rgb 200 200 200
                 else
                    Css.rgb 140 140 140
                )
            , Css.position Css.absolute
            , Css.top (Css.px (toFloat row * sqSize))
            , Css.left (Css.px (toFloat col * sqSize))
            , Css.width (Css.px sqSize)
            , Css.height (Css.px sqSize)
            ]
        , onClick msg
        ]
        [ case piece of
            Nothing ->
                text ""

            Just piece ->
                div
                    [ styles
                        [ Css.position Css.absolute
                        , Css.width (Css.px sqSize)
                        , Css.height (Css.px sqSize)
                        , Css.backgroundImage (Css.url (pieceImgUrl piece))
                        , Css.backgroundSize2 (Css.px sqSize) (Css.px sqSize)
                        ]
                    ]
                    []
        ]


squareToCoordinates : Square -> Bool -> ( Int, Int )
squareToCoordinates square isRotated =
    ( if isRotated then
        7 - (square |> Square.file |> File.toIndex)
      else
        square |> Square.file |> File.toIndex
    , if isRotated then
        square |> Square.rank |> Rank.toIndex
      else
        7 - (square |> Square.rank |> Rank.toIndex)
    )


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


pieceImgUrl : Piece -> String
pieceImgUrl piece =
    imgUrlPrefix
        ++ (piece |> Piece.color |> PieceColor.toString)
        ++ (piece |> Piece.kind |> PieceType.toString |> String.toLower)
        ++ ".png"


imgUrlPrefix : String
imgUrlPrefix =
    "http://res.cloudinary.com/ds1kquy7j/image/upload/"
