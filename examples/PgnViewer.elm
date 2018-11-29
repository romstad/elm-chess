module PgnViewer exposing (Model, Msg(..), board, boardOrError, imgUrlPrefix, init, main, pgnInputBox, pieceImgUrl, square, squareToCoordinates, update, view)

import Browser
import Css
import Game exposing (Game)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (class, cols, css, placeholder, rows)
import Html.Styled.Events exposing (onClick, onInput)
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
    Maybe Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( Just Game.empty, Cmd.none )



-- UPDATE


type Msg
    = TextInput String
    | Back
    | Forward


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextInput pgn ->
            ( Game.fromPgn pgn, Cmd.none )

        Back ->
            case model of
                Just game ->
                    ( Just (Game.back game), Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Forward ->
            case model of
                Just game ->
                    ( Just (Game.forward game), Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ boardOrError model
        , div
            []
            [ button [ onClick Back ] [ text "Back" ]
            , button [ onClick Forward ] [ text "Forward" ]
            ]
        , pgnInputBox
        ]


boardOrError : Model -> Html Msg
boardOrError model =
    case model of
        Just game ->
            board (Game.position game) 400.0

        Nothing ->
            div [] [ text "Invalid PGN" ]


pgnInputBox : Html Msg
pgnInputBox =
    div []
        [ textarea
            [ placeholder "Copy or type PGN game here."
            , rows 30
            , cols 80
            , onInput TextInput
            ]
            []
        ]


board : Position -> Float -> Html Msg
board position size =
    Html.div
        [ css
            [ Css.width (Css.px size)
            , Css.height (Css.px size)
            , Css.position Css.relative
            ]
        ]
        (List.map
            (\s ->
                square
                    (squareToCoordinates s)
                    (Position.pieceOn s position)
                    (size / 8)
            )
            Square.all
        )


square : ( Int, Int ) -> Maybe Piece -> Float -> Html Msg
square ( col, row ) piece sqSize =
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


squareToCoordinates : Square -> ( Int, Int )
squareToCoordinates square_ =
    ( square_ |> Square.file |> File.toIndex
    , 7 - (square_ |> Square.rank |> Rank.toIndex)
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
