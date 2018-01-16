module Main exposing (..)

import Html exposing (..)


type alias Model =
    Int


type Msg
    = NoOp


init =
    1


update message model =
    model


view model =
    div []
        [ text (toString 1)
        ]


main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
