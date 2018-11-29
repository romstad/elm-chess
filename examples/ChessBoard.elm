module ChessBoard exposing (Model, Msg(..), board, imgUrlPrefix, init, main, pieceImgUrl, square, squarePressed, squareToCoordinates, update, view)

import Browser
import Css
import Game exposing (Game)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Move exposing (Move)
import Piece exposing (Piece)
import PieceColor
import PieceType
import Position exposing (Position)
import Square exposing (Square)
import SquareFile as File
import SquareRank as Rank


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
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


init : () -> ( Model, Cmd Msg )
init _ =
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
                        Game.position model.game |> Position.movesFrom s
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
        [ css
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
        [ css
            [ Css.backgroundColor
                (if modBy 2 (col + row) == 0 then
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

            Just piece_ ->
                div
                    [ css
                        [ Css.position Css.absolute
                        , Css.width (Css.px sqSize)
                        , Css.height (Css.px sqSize)
                        , Css.backgroundImage (Css.url (pieceImgUrl piece_))
                        , Css.backgroundSize2 (Css.px sqSize) (Css.px sqSize)
                        ]
                    ]
                    []
        ]


squareToCoordinates : Square -> Bool -> ( Int, Int )
squareToCoordinates square_ isRotated =
    ( if isRotated then
        7 - (square_ |> Square.file |> File.toIndex)
      else
        square_ |> Square.file |> File.toIndex
    , if isRotated then
        square_ |> Square.rank |> Rank.toIndex
      else
        7 - (square_ |> Square.rank |> Rank.toIndex)
    )


pieceImgUrl : Piece -> String
pieceImgUrl piece =
    imgUrlPrefix
        ++ (piece |> Piece.color |> PieceColor.toString)
        ++ (piece |> Piece.kind |> PieceType.toString |> String.toLower)
        ++ ".png"


imgUrlPrefix : String
imgUrlPrefix =
    "http://res.cloudinary.com/ds1kquy7j/image/upload/"
