module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { gamesList : List Game
    , displayGamesList : Bool
    }


type alias Game =
    { gameTitle : String
    , gameDescription : String
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { gamesList =
        [ { gameTitle = "Platform Game", gameDescription = "Platform game example." }
        , { gameTitle = "Adventure Game", gameDescription = "Adventure game example." }
        ]
    , displayGamesList = False
    }


model : List String
model =
    [ "Platform Game"
    , "Adventure Game"
    ]



-- MODEL FUNCTIONS


firstGameMaybe : Maybe String
firstGameMaybe =
    List.head model


firstGameTitle : String
firstGameTitle =
    Maybe.withDefault "" firstGameMaybe



-- UPDATE


type Msg
    = DisplayGamesList
    | HideGamesList


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        DisplayGamesList ->
            ( { model | displayGamesList = True }, Cmd.none )

        HideGamesList ->
            ( { model | displayGamesList = False }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        gamesListDisplay =
            if model.displayGamesList then
                gamesIndex model
            else
                div [] []
    in
        div []
            [ h1 [ class "games-section" ] [ text "Games" ]
            , button [ class "btn btn-success", onClick DisplayGamesList ] [ text "Display Games List" ]
            , button [ class "btn btn-danger", onClick HideGamesList ] [ text "Hide Games List" ]
            , gamesListDisplay
            ]


gamesIndex : Model -> Html Msg
gamesIndex model =
    div [ class "games-index" ] [ gamesList model.gamesList ]


gamesList : List Game -> Html msg
gamesList games =
    ul [ class "games-list" ] (List.map gamesListItem games)


gamesListItem : Game -> Html msg
gamesListItem game =
    li [ class "game-item" ]
        [ strong [] [ text game.gameTitle ]
        , p [] [ text game.gameDescription ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
