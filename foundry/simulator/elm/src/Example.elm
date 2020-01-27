module Example exposing (main)

import Array exposing (Array)
import Bitwise
import IntWidths exposing (..)
import Interface exposing (..)
import Maybe exposing (withDefault)
import Maybe.Extra
import String


type alias SimState =
    { accum : IntW Num4
    , buffer : IntW Num8
    , inst : IntW Num8
    , pc : IntW Num4
    , dataMem : Array (IntW Num4)
    , progMem : Array (IntW Num8)
    }


performButton : Int -> SimState -> SimState
performButton n simState =
    case n of
        0 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 1)
            }

        1 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 2)
            }

        2 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 4)
            }

        3 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 8)
            }

        4 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 16)
            }

        5 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 32)
            }

        6 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 64)
            }

        7 ->
            { simState
                | buffer = binOpW Bitwise.xor simState.buffer (int8 128)
            }

        8 ->
            { simState
                | progMem = Array.set (toInt simState.pc) simState.buffer simState.progMem
            }

        9 ->
            { simState
                | buffer = int8 0
            }

        10 ->
            { simState
                | pc = binOpW (+) simState.pc (int4 1)
            }

        11 ->
            { simState
                | pc = binOpW (-) simState.pc (int4 1)
            }

        12 ->
            { simState
                | pc = int4 0
            }

        13 ->
            { simState
                | accum = int4 0
            }

        _ ->
            simState


getButtonName : Int -> String
getButtonName n =
    case n of
        0 ->
            "bufferI0"

        1 ->
            "bufferI1"

        2 ->
            "bufferI2"

        3 ->
            "bufferI3"

        4 ->
            "bufferI4"

        5 ->
            "bufferI5"

        6 ->
            "bufferI6"

        7 ->
            "bufferI7"

        8 ->
            "store"

        9 ->
            "clear"

        10 ->
            "pc_inc"

        11 ->
            "pc_dec"

        12 ->
            "pc_clr"

        13 ->
            "accum_clr"

        14 ->
            "run"

        15 ->
            "execute"

        _ ->
            String.fromInt n


type Inst
    = Halt
    | Ldi (IntW Num4)
    | Add (IntW Num4)
    | Sub (IntW Num4)
    | Ldm (IntW Num4)
    | Stm (IntW Num4)
    | Jp (IntW Num4)
    | Jpz (IntW Num4)


showInst : Inst -> String
showInst i =
    case i of
        Halt ->
            "halt"

        Ldi n ->
            "ldi " ++ (String.fromInt << toInt <| n)

        Add n ->
            "add " ++ (String.fromInt << toInt <| n)

        Sub n ->
            "sub " ++ (String.fromInt << toInt <| n)

        Ldm n ->
            "ldm " ++ (String.fromInt << toInt <| n)

        Stm n ->
            "stm " ++ (String.fromInt << toInt <| n)

        Jp n ->
            "jp " ++ (String.fromInt << toInt <| n)

        Jpz n ->
            "jpz " ++ (String.fromInt << toInt <| n)


readInst : String -> Maybe Inst
readInst s =
    case
        List.filter (not << String.isEmpty)
            << String.split " "
        <|
            s
    of
        [ "halt" ] ->
            Just Halt

        [ "ldi", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Ldi)

        [ "add", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Add)

        [ "sub", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Sub)

        [ "ldm", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Ldm)

        [ "stm", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Stm)

        [ "jp", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Jp)

        [ "jpz", n ] ->
            Maybe.Extra.andMap (Maybe.map int4 << String.toInt <| n) (Just Jpz)

        _ ->
            Nothing


decodeInst : IntW Num8 -> Inst
decodeInst x =
    case intToBits Little x of
        [ False, False, False, False, False, False, False, False ] ->
            Halt

        [ False, False, False, True, d1, d2, d3, d4 ] ->
            Ldi << bitsToInt Little <| [ d1, d2, d3, d4 ]

        [ False, False, True, False, d1, d2, d3, d4 ] ->
            Add << bitsToInt Little <| [ d1, d2, d3, d4 ]

        [ False, False, True, True, d1, d2, d3, d4 ] ->
            Sub << bitsToInt Little <| [ d1, d2, d3, d4 ]

        [ False, True, False, False, d1, d2, d3, d4 ] ->
            Ldm << bitsToInt Little <| [ d1, d2, d3, d4 ]

        [ False, True, False, True, d1, d2, d3, d4 ] ->
            Stm << bitsToInt Little <| [ d1, d2, d3, d4 ]

        [ False, True, True, False, d1, d2, d3, d4 ] ->
            Jp << bitsToInt Little <| [ d1, d2, d3, d4 ]

        [ False, True, True, True, d1, d2, d3, d4 ] ->
            Jpz << bitsToInt Little <| [ d1, d2, d3, d4 ]

        _ ->
            Halt


encodeInst : Inst -> IntW Num8
encodeInst i =
    case i of
        Halt ->
            bitsToInt Little [ False, False, False, False, False, False, False, False ]

        Ldi n ->
            concatBits4 (bitsToInt Little [ False, False, False, True ]) n

        Add n ->
            concatBits4 (bitsToInt Little [ False, False, True, False ]) n

        Sub n ->
            concatBits4 (bitsToInt Little [ False, False, True, True ]) n

        Ldm n ->
            concatBits4 (bitsToInt Little [ False, True, False, False ]) n

        Stm n ->
            concatBits4 (bitsToInt Little [ False, True, False, True ]) n

        Jp n ->
            concatBits4 (bitsToInt Little [ False, True, True, False ]) n

        Jpz n ->
            concatBits4 (bitsToInt Little [ False, True, True, True ]) n


tick : SimState -> TickRes SimState
tick simState =
    let
        simState_ =
            { simState
                | inst = withDefault (int8 0) (Array.get (toInt simState.pc) simState.progMem)
                , pc = binOpW (+) simState.pc (int4 1)
            }
    in
    let
        inst =
            decodeInst simState.inst
    in
    { debugMsg = showInst inst
    , newState =
        case inst of
            Halt ->
                { simState_
                    | pc = simState.pc
                }

            Ldi n ->
                { simState_
                    | accum = n
                }

            Add n ->
                { simState_
                    | accum = binOpW (+) simState.accum n
                }

            Sub n ->
                { simState_
                    | accum = binOpW (-) simState.accum n
                }

            Ldm n ->
                { simState_
                    | accum = withDefault (int4 0) (Array.get (toInt n) simState.dataMem)
                }

            Stm n ->
                { simState_
                    | dataMem = Array.set (toInt n) simState.accum simState.dataMem
                }

            Jp n ->
                { simState_
                    | pc = n
                }

            Jpz n ->
                { simState_
                    | pc =
                        if simState.accum == int4 0 then
                            n

                        else
                            binOpW (+) simState.pc (int4 1)
                }
    }


getLeds : SimState -> List Bool
getLeds simState =
    intToBits Little simState.buffer
        ++ intToBits Little (withDefault (int8 0) (Array.get (toInt simState.pc) simState.progMem))
        ++ intToBits Little simState.pc
        ++ intToBits Little simState.accum


getInspectibleMems : SimState -> List (InspectibleMem SimState)
getInspectibleMems simState =
    [ { name = "progMem"
      , contents =
            List.map
                (\n ->
                    { value =
                        Maybe.Extra.unwrap "" (showInst << decodeInst)
                            << Array.get n
                        <|
                            simState.progMem
                    , getHex = ( 8, Maybe.Extra.unwrap 0 toInt << Array.get n <| simState.progMem )
                    , set =
                        Maybe.map
                            (\i ->
                                { simState
                                    | progMem = Array.set n (encodeInst i) simState.progMem
                                }
                            )
                            << readInst
                    , selected = simState.pc == int4 n
                    }
                )
                (List.range 0 ((2 ^ 4) - 1))
      , setAll =
            \xs ->
                { simState
                    | progMem =
                        Array.fromList
                            << List.indexedMap
                                (\n ->
                                    Maybe.Extra.unwrap
                                        (withDefault
                                            (int8 0)
                                            (Array.get n simState.progMem)
                                        )
                                        encodeInst
                                        << readInst
                                )
                        <|
                            xs
                }
      , setAllHex =
            \xs ->
                { simState
                    | progMem = Array.fromList << List.map int8 <| xs
                }
      }
    , { name = "dataMem"
      , contents =
            List.map
                (\n ->
                    { value =
                        Maybe.Extra.unwrap "" (String.fromInt << toInt)
                            << Array.get n
                        <|
                            simState.dataMem
                    , getHex = ( 4, Maybe.Extra.unwrap 0 toInt << Array.get n <| simState.dataMem )
                    , set =
                        Maybe.map
                            (\x ->
                                { simState
                                    | dataMem = Array.set n (int4 x) simState.dataMem
                                }
                            )
                            << String.toInt
                    , selected = False
                    }
                )
                (List.range 0 ((2 ^ 4) - 1))
      , setAll =
            \xs ->
                { simState
                    | dataMem =
                        Array.fromList
                            << List.indexedMap
                                (\n ->
                                    Maybe.Extra.unwrap
                                        (withDefault
                                            (int4 0)
                                            (Array.get n simState.dataMem)
                                        )
                                        int4
                                        << String.toInt
                                )
                        <|
                            xs
                }
      , setAllHex =
            \xs ->
                { simState
                    | dataMem = Array.fromList << List.map int4 <| xs
                }
      }
    ]


sim : Sim SimState
sim =
    { defaultState =
        { accum = int4 0
        , buffer = int8 0
        , inst = int8 0
        , pc = int4 0
        , dataMem = Array.repeat (2 ^ 4) (int4 0)
        , progMem = Array.repeat (2 ^ 4) (int8 0)
        }
    , performButton = performButton
    , getButtonName = getButtonName
    , runButton = 14
    , executeButton = 15
    , tick = tick
    , getLeds = getLeds
    , getInspectibleMems = getInspectibleMems
    }


main : Interface SimState
main =
    interface sim
