module Main exposing (..)

import AnimationFrame exposing (times)
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Keyboard.Extra exposing (Key)
import Random exposing (initialSeed, step)
import Random.List exposing (choose, shuffle)
import Svg
import Svg.Attributes exposing (cx, cy, d, r, transform)
import Time exposing (Time)


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
    , startTime : Time
    }


type GameMsg
    = Undo
    | Clear
    | ChooseLetter Int
    | ShowHint
    | Submit
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
    , seed : Random.Seed
    , currentTime : Time
    }


type Msg
    = GoToTitle
    | StartNewGame
    | NewWord
    | GoToGameOver String
    | GameMsgContainer GameMsg
    | Tick Time
    | KeyUp Key


init : Int -> ( Model, Cmd msg )
init timestamp =
    ( Model TitleScreen (initialSeed timestamp) (toFloat timestamp), Cmd.none )


words =
    [ ( "JABBERWOCKY", "The jaws that bite, the claws that catch!" )
    , ( "MAXIMIZED", "Make as large or great as possible." )
    , ( "JACKKNIFE", "Move one's body into a bent or doubled-up position." )
    , ( "FLAPJACKS", "Sweet dense cakes made from oats, golden syrup, and melted butter, served in rectangles." )
    , ( "DOUGH", "A thick, malleable mixture of flour and liquid, used for baking into bread" )
    , ( "REFLECTION", "An image seen in a mirror or shiny surface" )
    , ( "ABSTRACT", "Existing in thought or as an idea but not having a physical or concrete existence" )
    , ( "DIRECTOR", "A person who is in charge of an activity, department, or organization" )
    , ( "CURVE", "A line or outline which gradually deviates from being straight for some or all of its length" )
    , ( "VERSION", "A particular release of a piece of computer software" )
    , ( "DEMAND", "An insistent and peremptory request, made as of right" )
    ]


checkResult game =
    let
        proposedSolution =
            game.choosenLetters
                |> List.map (\( id, string ) -> string)
                |> String.concat
    in
    game.solution == proposedSolution


getNewWord seed =
    let
        wordGenerator =
            choose words

        ( ( newWord, _ ), newSeed ) =
            Random.step wordGenerator seed
    in
    case newWord of
        Just ( word, hint ) ->
            let
                letterGenerator =
                    shuffle (String.split "" word)

                ( letters, newSeed2 ) =
                    Random.step letterGenerator newSeed
            in
            ( ( letters |> List.indexedMap (,), word, hint ), newSeed2 )

        Nothing ->
            Debug.crash "There are no words to choose from..."


newGame seed time =
    let
        ( ( letters, word, hint ), newSeed ) =
            getNewWord seed
    in
    ( Game letters word hint [] False time, newSeed )


update message model =
    case message of
        GoToTitle ->
            ( { model | screen = TitleScreen }, Cmd.none )

        StartNewGame ->
            let
                ( gameModel, seed ) =
                    newGame model.seed model.currentTime
            in
            ( { model
                | screen = GameScreen gameModel
                , seed = seed
              }
            , Cmd.none
            )

        NewWord ->
            let
                ( gameModel, seed ) =
                    newGame model.seed model.currentTime
            in
            ( { model
                | screen = GameScreen gameModel
                , seed = seed
              }
            , Cmd.none
            )

        GoToGameOver gameOverMessage ->
            ( { model | screen = GameOverScreen gameOverMessage }, Cmd.none )

        Tick time ->
            case model.screen of
                GameScreen game ->
                    let
                        percentageElapsed =
                            getPercentageTimeElapsed game.startTime time
                    in
                    if percentageElapsed == 100 then
                        { model | currentTime = time }
                            |> update (GoToGameOver "Whoops! You ran out of time!")
                    else
                        ( { model | currentTime = time }, Cmd.none )

                _ ->
                    ( { model | currentTime = time }, Cmd.none )

        KeyUp key ->
            case model.screen of
                GameScreen game ->
                    if key == Keyboard.Extra.BackSpace then
                        update (GameMsgContainer Undo) model
                    else
                        let
                            letter =
                                key
                                    |> Keyboard.Extra.toCode
                                    |> Char.fromCode
                                    |> String.fromChar
                                    |> String.toUpper

                            choosenLetter =
                                game.letters
                                    |> List.filter (\( id, str ) -> str == letter)
                                    |> List.map (\( id, _ ) -> id)
                                    |> List.head
                        in
                        case choosenLetter of
                            Just letterId ->
                                update (GameMsgContainer (ChooseLetter letterId)) model

                            Nothing ->
                                ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameMsgContainer gameMsg ->
            case model.screen of
                GameScreen game ->
                    case gameMsg of
                        Undo ->
                            ( { model | screen = GameScreen (undo game) }, Cmd.none )

                        Clear ->
                            ( { model | screen = GameScreen (clear game) }, Cmd.none )

                        ChooseLetter letterId ->
                            ( { model | screen = GameScreen (chooseLetter letterId game) }, Cmd.none )

                        ShowHint ->
                            ( { model | screen = GameScreen (showHint game) }, Cmd.none )

                        Submit ->
                            if checkResult game then
                                let
                                    ( gameModel, seed ) =
                                        newGame model.seed model.currentTime
                                in
                                ( { model
                                    | screen = GameScreen gameModel
                                    , seed = seed
                                  }
                                , Cmd.none
                                )
                            else
                                update (GoToGameOver "Whoops! That was the wrong word!") model

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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


getPercentageTimeElapsed startTime currentTime =
    let
        allowedTime =
            25 * 1000

        timeElapsed =
            currentTime - startTime
    in
    clamp 0 100 ((timeElapsed / allowedTime) * 100)


timer startTime currentTime =
    let
        percentageElapsed =
            getPercentageTimeElapsed startTime currentTime

        rotation =
            (percentageElapsed / 100) * 360

        rotationString =
            "rotate(" ++ toString rotation ++ " 100 100)"
    in
    div []
        [ Svg.svg [ style [ ( "width", "200px" ), ( "height", "200px" ) ] ]
            [ Svg.circle [ cx "100px", cy "100px", r "90px" ] []
            , Svg.path [ d "M100 100 L100 30", transform rotationString ] []
            ]
        ]


letterBlock selectable letterWidth listOfUsedIds ( id, letter ) =
    let
        isDisabled =
            selectable && List.member id listOfUsedIds
    in
    div [ classList [ ( "letter no-select", True ), ( "letter--disabled", isDisabled ) ], onClick (ChooseLetter id), style [ ( "width", letterWidth ) ] ]
        [ div [] [ text letter ]
        ]


letterBlocks selectable totalLetterCount letters listOfUsedIds =
    let
        containerMaxWidth =
            clamp 200 600 (totalLetterCount * 60)

        containerMaxWidthString =
            toString containerMaxWidth ++ "px"

        letterWidth =
            "calc((100% - (" ++ toString (totalLetterCount - 1) ++ " * 5px)) / " ++ toString totalLetterCount ++ ")"
    in
    div [ classList [ ( "letter-container", True ), ( "letter-container--selectable", selectable ) ], style [ ( "max-width", containerMaxWidthString ) ] ] (List.map (letterBlock selectable letterWidth listOfUsedIds) letters)


gameScreen : Game -> Time -> Html GameMsg
gameScreen game currentTime =
    let
        canUndo =
            List.length game.choosenLetters > 0

        canClear =
            List.length game.choosenLetters > 0

        canSubmit =
            List.length game.choosenLetters == totalLetterCount

        canShowHint =
            not game.showHint

        someLettersChoosen =
            List.length game.choosenLetters > 0

        allLetters =
            game.letters ++ game.choosenLetters

        totalLetterCount =
            List.length allLetters

        listOfUsedIds =
            List.map (\( id, letter ) -> id) game.choosenLetters
    in
    div []
        [ div [ class "timer-container" ] [ timer game.startTime currentTime ]
        , letterBlocks True totalLetterCount (List.sortWith (\( id, _ ) ( id2, _ ) -> compare id id2) allLetters) listOfUsedIds
        , br [] []
        , if someLettersChoosen then
            letterBlocks False totalLetterCount game.choosenLetters listOfUsedIds
          else
            div [ class "instructions-container" ] [ text "Click or type the letters above to solve the anagram before the time runs out!" ]
        , br [] []
        , buttonSet
            [ button "Undo" Normal (not canUndo) Undo
            , button "Clear" Danger (not canClear) Clear
            , button "Submit" Success (not canSubmit) Submit
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
                Html.map (\msg -> GameMsgContainer msg) (gameScreen game model.currentTime)

            GameOverScreen message ->
                gameOverScreen message
        ]


main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ times (\time -> Tick time)
                    , Keyboard.Extra.ups KeyUp
                    ]
        }
