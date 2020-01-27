module Test.Extra exposing (equalFn)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


equalFn : Fuzzer a -> String -> (a -> b) -> (a -> b) -> Test
equalFn fuzzer name f g =
    Test.fuzz fuzzer name <| \x -> Expect.equal (f x) (g x)
