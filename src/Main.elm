module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import List.Extra
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { wordOfTheDay : List Char
    , board : List (List Cell)
    , currentRow : Int
    , gameStatus : GameState
    }


type GameState
    = Won
    | Lost
    | Playing


type alias Cell =
    { letter : Maybe Char
    , status : CellStatus
    }


wordBank : List (List Char)
wordBank =
    [ [ 'h', 'e', 'l', 'l', 'o' ]
    , [ 'w', 'o', 'r', 'l', 'd' ]
    , [ 'h', 'a', 'p', 'p', 'y' ]
    , [ 'f', 'u', 'n', 'n', 'y' ]
    , [ 'g', 'a', 'm', 'm', 'a' ]
    , [ 's', 'p', 'r', 'a', 'y' ]
    , [ 'a', 'p', 'p', 'l', 'e' ]
    , [ 'm', 'a', 'n', 'g', 'o' ]
    , [ 'h', 'o', 'u', 's', 'e' ]
    , [ 'b', 'l', 'i', 'n', 'd' ]
    ]


getRandomWord : List (List Char) -> Cmd Msg
getRandomWord bank =
    let
        indexGenerator =
            Random.int 0 (List.length bank)
    in
    Random.generate (\index -> List.Extra.getAt index bank |> Maybe.withDefault [] |> WordOfTheDay) indexGenerator


type CellStatus
    = Correct
    | Misplaced
    | Incorrect
    | Empty


emptyCell : Cell
emptyCell =
    { letter = Nothing, status = Empty }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { wordOfTheDay = []
      , board = List.repeat 6 (List.repeat 5 emptyCell)
      , currentRow = 0
      , gameStatus = Playing
      }
    , getRandomWord wordBank
    )


type Msg
    = KeyPressed String
    | Submit
    | Backspace
    | WordOfTheDay (List Char)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed char ->
            ( handleKeyPressed char model, Cmd.none )

        Submit ->
            ( handleSubmit model, Cmd.none )

        Backspace ->
            ( handleBackspace model, Cmd.none )

        WordOfTheDay word ->
            let
                _ =
                    Debug.log "word" word
            in
            ( { model | wordOfTheDay = word }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ div [ class "board" ]
            (List.map (renderRow model) model.board)
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Decode.map toMsg keyDecoder)


toMsg : Key -> Msg
toMsg key =
    case key of
        Character char ->
            KeyPressed (char |> String.fromChar)

        BackspaceKey ->
            Backspace

        EnterKey ->
            Submit


type Key
    = Character Char
    | BackspaceKey
    | EnterKey


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    case string of
        "Backspace" ->
            BackspaceKey

        "Enter" ->
            EnterKey

        _ ->
            case String.uncons string of
                Just ( char, "" ) ->
                    if Char.isAlpha char then
                        Character (Char.toLower char)

                    else
                        Debug.log "Non-alpha key" (Character ' ')

                _ ->
                    Debug.log "Invalid key" (Character ' ')


renderRow : Model -> List Cell -> Html Msg
renderRow _ row =
    div [ class "row" ]
        (List.map renderCell row)


renderCell : Cell -> Html Msg
renderCell cell =
    div
        [ class "cell"
        , class (getCellStatusClass cell.status)
        ]
        [ text (String.fromChar (Maybe.withDefault ' ' cell.letter)) ]


getCellStatusClass : CellStatus -> String
getCellStatusClass status =
    case status of
        Correct ->
            "correct"

        Misplaced ->
            "misplaced"

        Incorrect ->
            "incorrect"

        Empty ->
            "empty"


handleSubmit : Model -> Model
handleSubmit model =
    let
        currentRow =
            List.Extra.getAt model.currentRow model.board
                |> Maybe.withDefault []

        isRowComplete =
            List.all (\cell -> cell.letter /= Nothing) currentRow
    in
    if not isRowComplete then
        model

    else
        let
            updatedRow =
                List.map2
                    (\cell targetChar ->
                        case cell.letter of
                            Just guessChar ->
                                { cell
                                    | status =
                                        if guessChar == targetChar then
                                            Correct

                                        else if List.member guessChar model.wordOfTheDay then
                                            Misplaced

                                        else
                                            Incorrect
                                }

                            Nothing ->
                                cell
                    )
                    currentRow
                    model.wordOfTheDay

            isCorrect =
                List.all (\cell -> cell.status == Correct) updatedRow

            updatedBoard =
                List.indexedMap
                    (\index row ->
                        if index == model.currentRow then
                            updatedRow

                        else
                            row
                    )
                    model.board

            updatedModel =
                { model
                    | board = updatedBoard
                    , currentRow =
                        if isCorrect then
                            model.currentRow

                        else
                            model.currentRow + 1
                    , gameStatus =
                        if isCorrect then
                            Won

                        else if model.currentRow >= 5 then
                            Lost

                        else
                            Playing
                }
        in
        updatedModel


handleBackspace : Model -> Model
handleBackspace model =
    { model
        | board =
            List.indexedMap
                (\rowIndex row ->
                    if rowIndex == model.currentRow then
                        List.foldr
                            (\cell ( acc, found ) ->
                                if not found && cell.letter /= Nothing then
                                    ( { cell | letter = Nothing } :: acc, True )

                                else
                                    ( cell :: acc, found )
                            )
                            ( [], False )
                            row
                            |> Tuple.first

                    else
                        row
                )
                model.board
    }


handleKeyPressed : String -> Model -> Model
handleKeyPressed char model =
    -- Only update if the game is still in progress
    if model.gameStatus /= Playing then
        model

    else
        let
            currentRow =
                model.currentRow

            updatedBoard =
                List.indexedMap
                    (\rowIndex row ->
                        if rowIndex == currentRow then
                            updateRow char row

                        else
                            row
                    )
                    model.board
        in
        { model | board = updatedBoard }


updateRow : String -> List Cell -> List Cell
updateRow char row =
    let
        firstEmptyIndex =
            List.Extra.findIndex (\cell -> cell.letter == Nothing) row
    in
    case firstEmptyIndex of
        Just index ->
            List.indexedMap
                (\i cell ->
                    if i == index then
                        { cell | letter = Just (char |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' ') }

                    else
                        cell
                )
                row

        Nothing ->
            row
