module Test.IntWidths exposing (suite)

import Basics.Extra exposing (flip, uncurry)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import IntWidths exposing (..)
import Test exposing (Test)
import Test.Extra


zipF : (a1 -> a2) -> (b1 -> b2) -> ( a1, b1 ) -> ( a2, b2 )
zipF f g ( x, y ) =
    ( f x, g y )


fuzzListN : Int -> Fuzzer a -> Fuzzer (List a)
fuzzListN n fuzzer =
    if n == 0 then
        Fuzz.constant []

    else
        Fuzz.map2 (::) fuzzer (fuzzListN (n - 1) fuzzer)


type alias TestOnIntW a =
    ( Int, Int -> IntW a ) -> Test


test_intW_toInt : TestOnIntW a
test_intW_toInt ( w, intW ) =
    Test.Extra.equalFn
        (Fuzz.map intW Fuzz.int)
        ("int" ++ String.fromInt w ++ " << toInt")
        identity
        (intW << toInt)


test_toInt_intW : TestOnIntW a
test_toInt_intW ( w, intW ) =
    Test.Extra.equalFn
        Fuzz.int
        ("toInt << int" ++ String.fromInt w)
        (modBy (2 ^ w))
        (toInt << intW)


test_binOpW : TestOnIntW a
test_binOpW ( w, intW ) =
    Test.Extra.equalFn
        (Fuzz.tuple ( Fuzz.map intW Fuzz.int, Fuzz.map intW Fuzz.int ))
        ("binOpW (+) on int" ++ String.fromInt w)
        (intW << uncurry (+) << zipF toInt toInt)
        (uncurry (binOpW (+)))


test_bitsToInt_intToBits_Little : TestOnIntW a
test_bitsToInt_intToBits_Little ( w, intW ) =
    Test.Extra.equalFn
        (Fuzz.map intW Fuzz.int)
        ("bitsToInt Little << intToBits Little on Int" ++ String.fromInt w)
        identity
        (bitsToInt Little << intToBits Little)


test_intToBits_bitsToInt_Little : TestOnIntW a
test_intToBits_bitsToInt_Little ( w, intW ) =
    Test.Extra.equalFn
        (fuzzListN w Fuzz.bool)
        ("intToBits Little << bitsToInt Little on Lists of length " ++ String.fromInt w)
        identity
        (intToBits Little << bitsToInt Little)


test_bitsToInt_intToBits_Big : TestOnIntW a
test_bitsToInt_intToBits_Big ( w, intW ) =
    Test.Extra.equalFn
        (Fuzz.map intW Fuzz.int)
        ("bitsToInt Big << intToBits Big on Int" ++ String.fromInt w)
        identity
        (bitsToInt Big << intToBits Big)


test_intToBits_bitsToInt_Big : TestOnIntW a
test_intToBits_bitsToInt_Big ( w, intW ) =
    Test.Extra.equalFn
        (fuzzListN w Fuzz.bool)
        ("intToBits Big << bitsToInt Big on Lists of length " ++ String.fromInt w)
        identity
        (intToBits Big << bitsToInt Big)


test_concatBitsW : TestOnIntW a
test_concatBitsW ( w, intW ) =
    Test.describe
        ("uncurry concatBitsW on Int" ++ String.fromInt w)
        [ Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int0 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits0 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits0)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int1 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits1 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits1)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int2 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits2 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits2)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int3 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits3 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits3)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int4 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits4 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits4)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int5 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits5 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits5)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int6 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits6 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits6)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int7 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits7 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits7)
        , Test.Extra.equalFn
            (Fuzz.tuple ( Fuzz.map int8 Fuzz.int, Fuzz.map intW Fuzz.int ))
            ("uncurry concatBits8 on Int" ++ String.fromInt w)
            (bitsToInt Little << uncurry (++) << zipF (intToBits Little) (intToBits Little))
            (uncurry concatBits8)
        ]


testsOnIntW : List (TestOnIntW a)
testsOnIntW =
    [ test_intW_toInt
    , test_toInt_intW
    , test_binOpW
    , test_bitsToInt_intToBits_Little
    , test_intToBits_bitsToInt_Little
    , test_bitsToInt_intToBits_Big
    , test_intToBits_bitsToInt_Big
    , test_concatBitsW
    ]


suite : Test
suite =
    Test.describe "module IntWidths"
        [ Test.concat (List.map ((|>) ( 0, int0 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 1, int1 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 2, int2 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 3, int3 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 4, int4 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 5, int5 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 6, int6 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 7, int7 )) testsOnIntW)
        , Test.concat (List.map ((|>) ( 8, int8 )) testsOnIntW)
        ]
