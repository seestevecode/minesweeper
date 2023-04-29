module Main exposing (main)

import Browser
import Element as Ui
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    String


type Msg
    = NoOp


init : Model
init =
    "Hello, Minesweeper!"


view : Model -> Html Msg
view model =
    Ui.layout [] <| Ui.text model


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
