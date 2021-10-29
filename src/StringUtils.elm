module StringUtils exposing (toCamelCase, toPascalCase)

import Set exposing (Set)


isAscii : Char -> Bool
isAscii char =
    Char.toCode char
        |> (\x -> 0 <= x && x <= 127)


cleanText : String -> List String
cleanText text =
    text
        |> String.filter isAscii
        |> String.split " "


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
    cleanText text
        |> List.map firstCharToUpper
        |> String.join ""
