module StringUtilsTest exposing (..)

import Expect
import StringUtils exposing (cleanAndSplitText, toCamelCase)
import Test exposing (..)


suite : Test
suite =
    describe "StringUtils"
        [ describe "cleanText"
            [ test "will keep only ascii chars" <|
                \_ ->
                    Expect.equal [ "abc" ] (cleanAndSplitText "abc$$")
            , test "will keep numbers" <|
                \_ ->
                    Expect.equal [ "123" ] (cleanAndSplitText "123$$")
            , test "will break on space" <|
                \_ ->
                    Expect.equal [ "abc", "def" ] (cleanAndSplitText "abc def")
            , test "will remove empty spaces" <|
                \_ ->
                    Expect.equal [ "abc", "def" ] (cleanAndSplitText "abc     def")
            , test "will break on UpperCase" <|
                \_ ->
                    Expect.equal [ "abc", "Def" ] (cleanAndSplitText "abcDef")
            , test "will break on -" <|
                \_ ->
                    Expect.equal [ "abc", "def" ] (cleanAndSplitText "abc-def")
            , test "will break on _" <|
                \_ ->
                    Expect.equal [ "abc", "def" ] (cleanAndSplitText "abc_def")
            , test "will break on edge cases" <|
                \_ ->
                    Expect.equal
                        [ "Hello"
                        , "World"
                        , "does"
                        , "this"
                        , "work"
                        ]
                        (cleanAndSplitText "HelloWorld-does this work")
            ]
        , describe "toCamelCase"
            [ test "will convert space separated text to camelCase" <|
                \_ ->
                    Expect.equal "helloWorld" (toCamelCase "hello world")
            , test "will convert comma separated text to camelCase" <|
                \_ ->
                    Expect.equal "helloWorld" (toCamelCase "hello,world")
            , test "will convert PascalCase to camelCase" <|
                \_ ->
                    Expect.equal "helloWorld" (toCamelCase "HelloWorld")
            , test "will convert hypen-case to camelCase" <|
                \_ ->
                    Expect.equal "helloWorld" (toCamelCase "hello-world")
            , test "will convert snake_case to camelCase" <|
                \_ ->
                    Expect.equal "helloWorld" (toCamelCase "hello_world")
            , test "will convert UPPER_SNAKE_CASE to camelCase" <|
                \_ ->
                    Expect.equal "helloWorld" (toCamelCase "HELLO_WORLD")
            ]
        ]
