-- Main is the entry point of this web application.


module Main exposing (..)

import Browser
import Html exposing (Html, div, input, label, option, select, text)
import Html.Attributes exposing (class, selected, type_, value)
import Html.Events exposing (onInput)
import StringUtils exposing (toCamelCase, toHypenCase, toPascalCase, toSnakeCase, toUpperSnakeCase)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { text = ""
    , mode = ToCamelCase
    }


type alias Model =
    { text : String
    , mode : Mode
    }


type Mode
    = ToCamelCase
    | ToPascalCase
    | ToHypenCase
    | ToSnakeCase
    | ToUpperSnakeCase


type Msg
    = OnChange String
    | OnOptionChange Mode


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnChange inputText ->
            { model | text = inputText }

        OnOptionChange mode ->
            { model | mode = mode }


transform : Mode -> String -> String
transform mode inputText =
    case mode of
        ToCamelCase ->
            toCamelCase inputText

        ToPascalCase ->
            toPascalCase inputText

        ToHypenCase ->
            toHypenCase inputText

        ToSnakeCase ->
            toSnakeCase inputText

        ToUpperSnakeCase ->
            toUpperSnakeCase inputText


renderOutput : Model -> Html a
renderOutput model =
    div
        [ class "border-2 p-16 h-60" ]
        [ model.text
            |> transform model.mode
            |> text
        ]


renderInput : Html Msg
renderInput =
    let
        labelForInput : Html a
        labelForInput =
            label [ class "col-3" ] [ text "Input text to convert" ]
    in
    div
        [ class "row mb-5" ]
        [ labelForInput
        , input
            [ class "col"
            , onInput OnChange
            , type_ "text"
            ]
            []
        ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        ToCamelCase ->
            "To camelCase"

        ToPascalCase ->
            "To PascalCase"

        ToHypenCase ->
            "To hypen-case"

        ToUpperSnakeCase ->
            "To UPPER_SNAKE_CASE"

        ToSnakeCase ->
            "To snake_case"


modeFromString : String -> Maybe Mode
modeFromString modeString =
    case modeString of
        "To camelCase" ->
            Just ToCamelCase

        "To PascalCase" ->
            Just ToPascalCase

        "To hypen-case" ->
            Just ToHypenCase

        "To snake_case" ->
            Just ToSnakeCase

        "To UPPER_SNAKE_CASE" ->
            Just ToUpperSnakeCase

        _ ->
            Nothing


renderMenu : Model -> Html Msg
renderMenu model =
    let
        renderOption : Mode -> Mode -> Html a
        renderOption selectedMode mode =
            let
                textLabel : String
                textLabel =
                    modeToString mode
            in
            option
                [ value textLabel
                , selected (selectedMode == mode)
                ]
                [ modeToString mode |> text ]

        menu : Html Msg
        menu =
            select
                [ class "col"
                , onInput <|
                    \val ->
                        modeFromString val
                            |> Maybe.withDefault model.mode
                            |> OnOptionChange
                ]
                [ renderOption model.mode ToCamelCase
                , renderOption model.mode ToPascalCase
                , renderOption model.mode ToHypenCase
                , renderOption model.mode ToUpperSnakeCase
                , renderOption model.mode ToSnakeCase
                ]
    in
    div [ class "row mb-5" ]
        [ label [ class "col-3" ] [ text "Options" ]
        , menu
        ]


view : Model -> Html Msg
view model =
    div [ class "container pt-5" ]
        [ renderMenu model
        , renderInput
        , renderOutput model
        ]
