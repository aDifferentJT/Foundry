module Test.Hex exposing (suite)

import Basics.Extra exposing (flip, uncurry)
import Char
import Expect exposing (Expectation)
import Expect.Extra
import Fuzz exposing (Fuzzer)
import Hex exposing (..)
import Random
import Test exposing (Test)
import Test.Extra


fuzzNibbles : Fuzzer Char
fuzzNibbles =
    Fuzz.oneOf
        << List.map Fuzz.constant
        << String.toList
    <|
        "0123456789abcdef"


fuzzHex : Fuzzer String
fuzzHex =
    Fuzz.map (String.fromList << List.take 8)
        << Fuzz.list
    <|
        fuzzNibbles


isHex : String -> Bool
isHex =
    let
        cs =
            String.toList "0123456789abcdef"
    in
    List.foldr (&&) True
        << List.map (flip List.member cs << Char.toLower)
        << String.toList


suite : Test
suite =
    Test.describe "module Hex"
        [ Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.intRange 0 32, Fuzz.intRange Random.minInt Random.maxInt ))
            "read << show"
            (uncurry <| \w -> read << show w)
            (uncurry <| \_ -> Just)
        , Test.Extra.equalFn
            fuzzHex
            "show << read"
            (\s -> Maybe.map (show (String.length s * 4)) << read <| s)
            Just
        , Test.test "show 8 1 == \"01\"" <| \_ -> Expect.equal "01" << show 8 <| 1
        , Test.test "read \"01\" == 1" <| \_ -> Expect.equal (Just 1) << read <| "01"
        , Test.fuzz Fuzz.string "read nonHex == Nothing" <|
            \s ->
                Expect.Extra.implies (not << isHex <| s) <|
                    \_ ->
                        Expect.equal Nothing (read s)
        ]
