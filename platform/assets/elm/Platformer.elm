module Platformer exposing (..)

import AnimationFrame exposing (diffs)
import Html exposing (Html, div)
import Keyboard exposing (KeyCode, downs)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)


-- MODEL


type GameState
    = StartScreen
    | Playing
    | Success
    | GameOver


type Direction
    = Left
    | Right


type alias Model =
    { gameState : GameState
    , characterDirection : Direction
    , characterPositionX : Int
    , characterPositionY : Int
    , itemPositionX : Int
    , itemPositionY : Int
    , itemsCollected : Int
    , playerScore : Int
    , timeRemaining : Int
    }


initialModel : Model
initialModel =
    { gameState = StartScreen
    , characterDirection = Right
    , characterPositionX = 50
    , characterPositionY = 300
    , itemPositionX = 500
    , itemPositionY = 300
    , itemsCollected = 0
    , playerScore = 0
    , timeRemaining = 10
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | CountdownTimer Time
    | KeyDown KeyCode
    | TimeUpdate Time
    | SetNewItemPositionX Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TimeUpdate time ->
            if characterFoundItem model then
                ( { model
                    | itemsCollected = model.itemsCollected + 1
                    , playerScore = model.playerScore + 100
                  }
                , Random.generate SetNewItemPositionX (Random.int 50 500)
                )
            else if model.itemsCollected >= 10 then
                ( { model | gameState = Success }, Cmd.none )
            else if model.itemsCollected < 10 && model.timeRemaining == 0 then
                ( { model | gameState = GameOver }, Cmd.none )
            else
                ( model, Cmd.none )

        CountdownTimer time ->
            if model.gameState == Playing && model.timeRemaining > 0 then
                ( { model | timeRemaining = model.timeRemaining - 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        SetNewItemPositionX newPositionX ->
            ( { model | itemPositionX = newPositionX }, Cmd.none )

        KeyDown keyCode ->
            case keyCode of
                32 ->
                    case model.gameState of
                        StartScreen ->
                            ( { model | gameState = Playing }, Cmd.none )

                        _ ->
                            ( initialModel, Cmd.none )

                37 ->
                    if model.gameState == Playing then
                        ( { model
                            | characterDirection = Left
                            , characterPositionX = model.characterPositionX - 15
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                39 ->
                    if model.gameState == Playing then
                        ( { model
                            | characterDirection = Right
                            , characterPositionX = model.characterPositionX + 15
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ downs KeyDown
        , diffs TimeUpdate
        , every second CountdownTimer
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ viewGame model ]


viewGameState : Model -> List (Svg Msg)
viewGameState model =
    case model.gameState of
        StartScreen ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewGameCharacter model
            , viewItem model
            , viewStartScreenText
            ]

        Playing ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewGameCharacter model
            , viewItem model
            , viewGameScore model
            , viewItemsCollected model
            , viewGameTime model
            ]

        Success ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewGameCharacter model
            , viewItem model
            , viewSuccessScreenText
            ]

        GameOver ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewGameCharacter model
            , viewItem model
            , viewGameOverText
            ]


viewGame : Model -> Svg Msg
viewGame model =
    svg [ version "1.1", width "600", height "400" ]
        (viewGameState model)


viewGameWindow : Svg Msg
viewGameWindow =
    rect
        [ width "600"
        , height "400"
        , fill "none"
        , stroke "black"
        ]
        []


viewGameText : Int -> Int -> String -> Svg Msg
viewGameText positionX positionY str =
    Svg.text_
        [ x (toString positionX)
        , y (toString positionY)
        , fontFamily "Courier"
        , fontWeight "bold"
        , fontSize "16"
        ]
        [ Svg.text str ]


viewStartScreenText : Svg Msg
viewStartScreenText =
    Svg.svg []
        [ viewGameText 140 160 "Collect ten coins in ten seconds!"
        , viewGameText 140 180 "Press the SPACE BAR key to restart"
        ]


viewSuccessScreenText : Svg Msg
viewSuccessScreenText =
    Svg.svg []
        [ viewGameText 260 180 "Success!" ]


viewGameOverText : Svg Msg
viewGameOverText =
    Svg.svg []
        [ viewGameText 260 160 "Game Over"
        , viewGameText 140 180 "Press the SPACE BAR key to restart."
        ]


viewGameScore : Model -> Svg Msg
viewGameScore model =
    let
        currentScore =
            model.playerScore
                |> toString
                |> String.padLeft 5 '0'
    in
        Svg.svg []
            [ viewGameText 25 25 "Score"
            , viewGameText 25 40 currentScore
            ]


viewGameTime : Model -> Svg Msg
viewGameTime model =
    let
        currentTime =
            model.timeRemaining
                |> toString
                |> String.padLeft 4 '0'
    in
        Svg.svg []
            [ viewGameText 525 25 "TIME"
            , viewGameText 525 40 currentTime
            ]


viewItemsCollected : Model -> Svg Msg
viewItemsCollected model =
    let
        currentItemCount =
            model.itemsCollected
                |> toString
                |> String.padLeft 3 '0'
    in
        Svg.svg []
            [ image
                [ xlinkHref "/images/dogeCoin.svg"
                , x "275"
                , y "18"
                , width "15"
                , height "15"
                ]
                []
            , viewGameText 300 30 currentItemCount
            ]


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
            model.itemPositionX - 35

        approximateItemUpperBound =
            model.itemPositionX + 10

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
