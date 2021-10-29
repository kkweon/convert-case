module StringUtils exposing
    ( cleanAndSplitText
    , toCamelCase
    , toHypenCase
    , toPascalCase
    , toSnakeCase
    , toUpperSnakeCase
    )

import Set exposing (Set)


{-| Important characters that should not be removed from the
text while cleaning.
-}
specialChars : Set Char
specialChars =
    Set.fromList [ ' ', '-', '_' ]


isUpperSnakeCase : String -> Bool
isUpperSnakeCase text =
    String.contains "_" text && String.toUpper text == text


isFirstCharUpperCase : String -> Bool
isFirstCharUpperCase text =
    case String.uncons text of
        Nothing ->
            False

        Just ( firstChar, _ ) ->
            Char.isUpper firstChar


breakString : String -> List String
breakString text =
    if isUpperSnakeCase text then
        String.split "_" text

    else
        let
            -- When char is a upper char, it checks the previous word is
            -- starting with an uppercase char. If so, it will append to the previous word.
            -- For example, in the following example,
            --
            -- c = 'Y',
            -- word = ""
            -- words = ["TMP", "Experiments"]
            --
            -- it will be transformed to ("", ["YTMP", "Experiments"])
            handleUpperCharCase : Char -> String -> List String -> ( String, List String )
            handleUpperCharCase c word words =
                case words of
                    [] ->
                        ( "", String.cons c word :: words )

                    prevWord :: rest ->
                        if isFirstCharUpperCase prevWord && String.isEmpty word then
                            ( "", String.cons c prevWord :: rest )

                        else
                            ( "", String.cons c word :: words )

            ( currentWord, result ) =
                String.foldr
                    (\c ( word, words ) ->
                        if Set.member c specialChars then
                            ( "", word :: words )

                        else if Char.isUpper c then
                            handleUpperCharCase c word words

                        else
                            ( String.cons c word, words )
                    )
                    ( "", [] )
                    text
        in
        if currentWord == "" then
            result

        else
            currentWord :: result


{-| Returns true if the character should be kept.
-}
isImportant : Char -> Bool
isImportant char =
    Char.isAlphaNum char || Set.member char specialChars


cleanAndSplitText : String -> List String
cleanAndSplitText text =
    text
        |> String.map
            (\c ->
                if isImportant c then
                    c

                else
                    ' '
            )
        |> breakString
        |> List.filter (\word -> String.isEmpty word |> not)


firstCharMap : (Char -> Char) -> String -> String
firstCharMap f text =
    case String.uncons text of
        Nothing ->
            text

        Just ( firstChar, rest ) ->
            firstChar |> f |> (\x -> String.cons x rest)


firstCharToUpper : String -> String
firstCharToUpper text =
    firstCharMap Char.toUpper text


firstCharToLower : String -> String
firstCharToLower text =
    firstCharMap Char.toLower text


toCamelCase : String -> String
toCamelCase text =
    toPascalCase text
        |> firstCharToLower


toPascalCase : String -> String
toPascalCase text =
    cleanAndSplitText text
        |> List.map (String.toLower >> firstCharToUpper)
        |> String.join ""


toHypenCase : String -> String
toHypenCase text =
    cleanAndSplitText text
        |> List.map String.toLower
        |> String.join "-"


toUpperSnakeCase : String -> String
toUpperSnakeCase text =
    toSnakeCase text
        |> String.toUpper


toSnakeCase : String -> String
toSnakeCase text =
    cleanAndSplitText text
        |> List.map String.toLower
        |> String.join "_"
