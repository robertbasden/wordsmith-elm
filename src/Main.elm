module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type ButtonType
    = Normal
    | Success
    | Danger


type alias Game =
    { letters : List ( Int, String )
    , solution : String
    , hint : String
    , choosenLetters : List ( Int, String )
    , showHint : Bool
    }


type GameMsg
    = Undo
    | Clear
    | ChooseLetter Int
    | ShowHint
    | NoOp


undo game =
    { game
        | choosenLetters =
            game.choosenLetters
                |> List.reverse
                |> List.drop 1
                |> List.reverse
        , letters =
            game.letters
                ++ (game.choosenLetters
                        |> List.reverse
                        |> List.take 1
                   )
    }


clear game =
    { game
        | choosenLetters = []
        , letters = game.letters ++ game.choosenLetters
    }


chooseLetter choosenLetterId game =
    { game
        | choosenLetters = game.choosenLetters ++ List.filter (\( id, letter ) -> id == choosenLetterId) game.letters
        , letters = List.filter (\( id, letter ) -> id /= choosenLetterId) game.letters
    }


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


words =
    [ ( "MAXIMIZED", "Make as large or great as possible." )
    , ( "JACKKNIFE", "Move one's body into a bent or doubled-up position." )
    , ( "FLAPJACKS", "Sweet dense cakes made from oats, golden syrup, and melted butter, served in rectangles." )
    ]


getNewWord =
    let
        newWord =
            List.head words
    in
    case newWord of
        Just ( word, hint ) ->
            let
                letters =
                    String.split "" word
                        |> List.indexedMap (,)
            in
            ( letters, word, hint )

        Nothing ->
            Debug.crash "There are no words to choose from..."


newGame =
    let
        ( letters, word, hint ) =
            getNewWord
    in
    Game letters word hint [] False


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

                        NoOp ->
                            model

                _ ->
                    model


container children =
    div [ class "container" ] children


pageTitle title =
    h1 [ class "page-title" ] [ text title ]


pageText t =
    div [ class "page-text" ] [ text t ]


buttonSet buttons =
    div [ class "button-set " ] buttons


button t btnType d c =
    let
        btnClass =
            case btnType of
                Success ->
                    "button button--success"

                Danger ->
                    "button button--dangeer"

                _ ->
                    "button"
    in
    Html.button
        [ disabled d
        , onClick c
        , class btnClass
        ]
        [ text t ]


titleScreen =
    div []
        [ pageTitle "WORDSMITH"
        , pageText "Complete the anagram before the time runs out"
        , buttonSet
            [ button "Start Game" Success False StartNewGame
            ]
        ]


timer =
    div [] []


letterBlock selectable listOfUsedIds ( id, letter ) =
    let
        isDisabled =
            selectable && List.member id listOfUsedIds
    in
    div [ classList [ ( "letter no-select", True ), ( "letter--disabled", isDisabled ) ], onClick (ChooseLetter id) ]
        [ div [] [ text letter ]
        ]


letterBlocks selectable letters listOfUsedIds =
    div [ classList [ ( "letter-container", True ), ( "letter-container--selectable", selectable ) ] ] (List.map (letterBlock selectable listOfUsedIds) letters)


gameScreen : Game -> Html GameMsg
gameScreen game =
    let
        canUndo =
            List.length game.choosenLetters > 0

        canClear =
            List.length game.choosenLetters > 0

        canSubmit =
            List.length game.choosenLetters == 9

        canShowHint =
            not game.showHint

        someLettersChoosen =
            List.length game.choosenLetters > 0

        allLetters =
            game.letters ++ game.choosenLetters

        listOfUsedIds =
            List.map (\( id, letter ) -> id) game.choosenLetters
    in
    div []
        [ timer
        , letterBlocks True (List.sortWith (\( id, _ ) ( id2, _ ) -> compare id id2) allLetters) listOfUsedIds
        , br [] []
        , if someLettersChoosen then
            letterBlocks False game.choosenLetters listOfUsedIds
          else
            div [ class "instructions-container" ] [ text "Click or type the letters above to solve the anagram before the time runs out!" ]
        , br [] []
        , buttonSet
            [ button "Undo" Normal (not canUndo) Undo
            , button "Clear" Danger (not canClear) Clear
            , button "Submit" Success (not canSubmit) NoOp
            , button "Show Hint" Normal (not canShowHint) ShowHint
            ]
        , br [] []
        , if game.showHint then
            div [ class "hint-container" ] [ strong [] [ text "Hint: " ], text game.hint ]
          else
            div [] [ text "" ]
        ]


gameOverScreen message =
    div []
        [ pageTitle "WHOOPS!"
        , pageText message
        , buttonSet
            [ button "Start Again?" Success False StartNewGame
            ]
        ]


view model =
    container
        [ case model.screen of
            TitleScreen ->
                titleScreen

            GameScreen game ->
                Html.map (\msg -> GameMsgContainer msg) (gameScreen game)

            GameOverScreen message ->
                gameOverScreen message
        ]


main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
