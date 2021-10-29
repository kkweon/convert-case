-- Main is the entry point of this web application.


module Main exposing (..)

import Browser
import Char exposing (toLower)
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (class, selected, value)
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
            Html.label [ class "mr-4" ] [ text "Input text to convert" ]
    in
    div
        [ class "flex" ]
        [ labelForInput
        , input
            [ class "flex-grow border-2"
            , onInput OnChange
            ]
            []
        ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        ToCamelCase ->
            "ToCamelCase"

        ToPascalCase ->
            "ToPascalCase"

        ToHypenCase ->
            "ToHypenCase"

        ToUpperSnakeCase ->
            "ToUpperSnakeCase"

        toSnakeCase ->
            "ToSnakeCase"


modeFromString : String -> Maybe Mode
modeFromString modeString =
    case modeString of
        "ToCamelCase" ->
            Just ToCamelCase

        "ToPascalCase" ->
            Just ToPascalCase

        "ToHypenCase" ->
            Just ToHypenCase

        "ToSnakeCase" ->
            Just ToSnakeCase

        "ToUpperSnakeCase" ->
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
                [ class "border-2"
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
    div [] [ menu ]


view : Model -> Html Msg
view model =
    div [ class "max-w-lg mx-auto pt-24 h-screen flex flex-col space-y-6" ]
        [ renderMenu model
        , renderInput
        , renderOutput model
        ]
