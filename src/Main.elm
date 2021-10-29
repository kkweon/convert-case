-- Main is the entry point of this web application.


module Main exposing (..)

import Browser
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import StringUtils exposing (toCamelCase, toPascalCase)


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


renderOutput : Model -> Html a
renderOutput model =
    div
        []
        [ model.text |> transform model.mode |> text ]


renderInput : Html Msg
renderInput =
    div
        [ class "flex justify-center" ]
        [ input
            [ class "border-2"
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


modeFromString : String -> Maybe Mode
modeFromString modeString =
    case modeString of
        "ToCamelCase" ->
            Just ToCamelCase

        "ToPascalCase" ->
            Just ToPascalCase

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
                [ onInput <|
                    \val ->
                        modeFromString val
                            |> Maybe.withDefault model.mode
                            |> OnOptionChange
                ]
                [ renderOption model.mode ToCamelCase
                , renderOption model.mode ToPascalCase
                ]
    in
    div [] [ menu ]


view : Model -> Html Msg
view model =
    div [ class "max-w-lg mx-auto pt-24" ]
        [ renderMenu model
        , renderInput
        , renderOutput model
        ]
