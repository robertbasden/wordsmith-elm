module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Html.Events exposing (onClick)


type alias Game =
    { letters : List (Int String)
    , solution : String
    , hint : String
    , choosenLetters : List (Int String)
    , showHint : Bool
    }


type GameMsg
    = Undo
    | Clear
    | ChooseLetter Int
    | ShowHint


undo game =
    game


clear game =
    game


chooseLetter choosenLetter game =
    game


showHint game =
    { game | showHint = True }


type Screen
    = TitleScreen
    | GameScreen Game
    | GameOverScreen String


type alias Model =
    { screen : Screen
    }


type Msg
    = GoToTitle
    | StartNewGame
    | NewWord
    | GoToGameOver String
    | GameMsgContainer GameMsg


init =
    Model TitleScreen


newGame =
    Game [] "" "" [] False


update message model =
    case message of
        GoToTitle ->
            { model | screen = TitleScreen }

        StartNewGame ->
            { model | screen = GameScreen newGame }

        NewWord ->
            { model | screen = GameScreen newGame }

        GoToGameOver gameOverMessage ->
            { model | screen = GameOverScreen gameOverMessage }

        GameMsgContainer gameMsg ->
            case model.screen of
                GameScreen game ->
                    case gameMsg of
                        Undo ->
                            { model | screen = GameScreen (undo game) }

                        Clear ->
                            { model | screen = GameScreen (clear game) }

                        ChooseLetter letterId ->
                            { model | screen = GameScreen (chooseLetter letterId game) }

                        ShowHint ->
                            { model | screen = GameScreen (showHint game) }

                _ ->
                    model


container children =
    div [] children


pageTitle title =
    div [] [ text title ]


pageText t =
    div [] [ text t ]


buttonSet buttons =
    div [] buttons


button t d c =
    Html.button
        [ disabled d
        , onClick c
        ]
        [ text t ]


view model =
    container
        [ case model.screen of
            TitleScreen ->
                div []
                    [ pageTitle "WORDSMITH"
                    , pageText "Complete the anagram before the time runs out"
                    , buttonSet
                        [ button "Start Game" False StartNewGame
                        ]
                    ]

            GameScreen game ->
                div []
                    [ buttonSet
                        [ button "Submit" False (GoToGameOver "Whoops! That was the wrong word!") ]
                    ]

            GameOverScreen message ->
                div []
                    [ pageTitle "WHOOPS!"
                    , pageText message
                    , buttonSet
                        [ button "Start Again?" False StartNewGame
                        ]
                    ]
        ]


main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
