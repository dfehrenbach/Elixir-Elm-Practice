module Platformer exposing (..)

import Html exposing (Html, div)
import Keyboard exposing (KeyCode, downs)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL


type Direction
    = Left
    | Right


type alias Model =
    { characterDirection : Direction
    , characterPositionX : Int
    , characterPositionY : Int
    , itemPositionX : Int
    , itemPositionY : Int
    }


initialModel : Model
initialModel =
    { characterDirection = Right
    , characterPositionX = 50
    , characterPositionY = 300
    , itemPositionX = 500
    , itemPositionY = 300
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown keyCode ->
            case keyCode of
                37 ->
                    ( { model
                        | characterDirection = Left
                        , characterPositionX = model.characterPositionX - 15
                      }
                    , Cmd.none
                    )

                39 ->
                    ( { model
                        | characterDirection = Right
                        , characterPositionX = model.characterPositionX + 15
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ downs KeyDown ]



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ viewGame model ]


viewGame : Model -> Svg Msg
viewGame model =
    svg [ version "1.1", width "600", height "400" ]
        [ viewGameWindow
        , viewGameSky
        , viewGameGround
        , viewGameCharacter model
        , viewItem model
        ]


viewGameWindow : Svg Msg
viewGameWindow =
    rect
        [ width "600"
        , height "400"
        , fill "none"
        , stroke "black"
        ]
        []


viewGameSky : Svg Msg
viewGameSky =
    rect
        [ x "0"
        , y "0"
        , width "600"
        , height "300"
        , fill "#4b7cfb"
        ]
        []


viewGameGround : Svg Msg
viewGameGround =
    rect
        [ x "0"
        , y "300"
        , width "600"
        , height "100"
        , fill "green"
        ]
        []


viewGameCharacter : Model -> Svg Msg
viewGameCharacter model =
    let
        pandaImage =
            case model.characterDirection of
                Right ->
                    "/images/pandaRight.png"

                Left ->
                    "/images/pandaLeft.png"
    in
        image
            [ xlinkHref pandaImage
            , x (toString model.characterPositionX)
            , y (toString model.characterPositionY)
            , width "50"
            , height "50"
            ]
            []


viewItem : Model -> Svg Msg
viewItem model =
    case characterFoundItem model of
        True ->
            svg [] []

        False ->
            image
                [ xlinkHref "/images/dogeCoin.svg"
                , x (toString model.itemPositionX)
                , y (toString model.itemPositionY)
                , width "20"
                , height "20"
                ]
                []


characterFoundItem : Model -> Bool
characterFoundItem model =
    let
        approximateItemLowerBound =
            model.itemPositionX - 30

        approximateItemUpperBound =
            model.itemPositionX + 5

        approximateItemRange =
            List.range approximateItemLowerBound approximateItemUpperBound
    in
        List.member model.characterPositionX approximateItemRange


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
