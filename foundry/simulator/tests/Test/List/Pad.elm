module Test.List.Pad exposing (suite)

import Basics.Extra exposing (flip, uncurry)
import Expect exposing (Expectation)
import Expect.Extra
import Fuzz exposing (Fuzzer)
import List exposing (length, repeat, reverse, take)
import List.Pad exposing (..)
import Test exposing (Test)
import Test.Extra


suite : Test
suite =
    Test.describe "module List.Pad"
        [ Test.fuzz
            (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 1000, Fuzz.list Fuzz.int ))
            "length (padLeft p n xs) == n"
          <|
            \( p, n, xs ) ->
                Expect.equal n
                    << length
                <|
                    padLeft p n xs
        , Test.fuzz
            (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 1000, Fuzz.list Fuzz.int ))
            "length (padRight p n xs) == n"
          <|
            \( p, n, xs ) ->
                Expect.equal n
                    << length
                <|
                    padRight p n xs
        , Test.fuzz
            (Fuzz.tuple ( Fuzz.int, Fuzz.intRange 0 1000 ))
            "padLeft p n [] == repeat n p"
          <|
            \( p, n ) ->
                Expect.equal (repeat n p) <| padLeft p n []
        , Test.fuzz
            (Fuzz.tuple ( Fuzz.int, Fuzz.intRange 0 1000 ))
            "padRight p n [] == repeat n p"
          <|
            \( p, n ) ->
                Expect.equal (repeat n p) <| padRight p n []
        , Test.fuzz
            (Fuzz.tuple ( Fuzz.int, Fuzz.list Fuzz.int ))
            "padLeft p (length xs) xs == xs"
          <|
            \( p, xs ) ->
                Expect.equal xs <|
                    padLeft p (length xs) xs
        , Test.fuzz
            (Fuzz.tuple ( Fuzz.int, Fuzz.list Fuzz.int ))
            "padRight p (length xs) xs == xs"
          <|
            \( p, xs ) ->
                Expect.equal xs <|
                    padRight p (length xs) xs
        , Test.fuzz
            (Fuzz.tuple3 ( Fuzz.int, Fuzz.int, Fuzz.list Fuzz.int ))
            "n < length xs => padRight p n xs == take n xs"
          <|
            \( p, n_, xs ) ->
                Expect.Extra.implies (length xs /= 0)
                    (\_ ->
                        let
                            n =
                                modBy (length xs) n_
                        in
                        Expect.equal (take n xs) <|
                            padRight p n xs
                    )
        , Test.fuzz
            (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 1000, Fuzz.list Fuzz.int ))
            "padLeft p (n + length xs) xs == repeat n p ++ xs"
          <|
            \( p, n, xs ) ->
                Expect.equal (repeat n p ++ xs) <|
                    padLeft p (n + length xs) xs
        , Test.fuzz
            (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 1000, Fuzz.list Fuzz.int ))
            "padRight p (length xs + n) xs == xs ++ repeat n p"
          <|
            \( p, n, xs ) ->
                Expect.equal (xs ++ repeat n p) <|
                    padRight p (length xs + n) xs
        , Test.fuzz
            (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 1000, Fuzz.list Fuzz.int ))
            "padLeft p n xs == reverse << padRight p n << reverse <| xs"
          <|
            \( p, n, xs ) ->
                Expect.equal (padLeft p n xs)
                    << reverse
                    << padRight p n
                    << reverse
                <|
                    xs
        ]
