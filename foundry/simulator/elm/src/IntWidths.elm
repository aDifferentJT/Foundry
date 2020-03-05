module IntWidths exposing
    ( Endianness(..)
    , IntW
    , Num0
    , Num1
    , Num2
    , Num3
    , Num4
    , Num5
    , Num6
    , Num7
    , Num8
    , binOpW
    , bitsToInt
    , concatBits0
    , concatBits1
    , concatBits2
    , concatBits3
    , concatBits4
    , concatBits5
    , concatBits6
    , concatBits7
    , concatBits8
    , int0
    , int1
    , int2
    , int3
    , int4
    , int5
    , int6
    , int7
    , int8
    , intToBits
    , toInt
    )

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import List.Extra


type Zero
    = Num0 Never


type Succ a
    = Succ Never


type alias Add2 a =
    Succ (Succ a)


type alias Add3 a =
    Succ (Add2 a)


type alias Add4 a =
    Succ (Add3 a)


type alias Add5 a =
    Succ (Add4 a)


type alias Add6 a =
    Succ (Add5 a)


type alias Add7 a =
    Succ (Add6 a)


type alias Add8 a =
    Succ (Add7 a)


type alias Num0 =
    Zero


type alias Num1 =
    Succ Zero


type alias Num2 =
    Add2 Zero


type alias Num3 =
    Add3 Zero


type alias Num4 =
    Add4 Zero


type alias Num5 =
    Add5 Zero


type alias Num6 =
    Add6 Zero


type alias Num7 =
    Add7 Zero


type alias Num8 =
    Add8 Zero


type IntW a
    = IntW Int Int


binOpW : (Int -> Int -> Int) -> IntW a -> IntW a -> IntW a
binOpW f (IntW w x) (IntW _ y) =
    IntW w (modBy (2 ^ w) (f x y))


intW : Int -> Int -> IntW a
intW w =
    IntW w << modBy (2 ^ w)


int0 : Int -> IntW Num0
int0 =
    intW 0


int1 : Int -> IntW Num1
int1 =
    intW 1


int2 : Int -> IntW Num2
int2 =
    intW 2


int3 : Int -> IntW Num3
int3 =
    intW 3


int4 : Int -> IntW Num4
int4 =
    intW 4


int5 : Int -> IntW Num5
int5 =
    intW 5


int6 : Int -> IntW Num6
int6 =
    intW 6


int7 : Int -> IntW Num7
int7 =
    intW 7


int8 : Int -> IntW Num8
int8 =
    intW 8


concatBits : IntW a -> IntW b -> IntW c
concatBits (IntW w1 x) (IntW w2 y) =
    IntW (w1 + w2) (Bitwise.or x (shiftLeftBy w1 y))


concatBits0 : IntW Num0 -> IntW a -> IntW a
concatBits0 =
    concatBits


concatBits1 : IntW Num1 -> IntW a -> IntW (Succ a)
concatBits1 =
    concatBits


concatBits2 : IntW Num2 -> IntW a -> IntW (Add2 a)
concatBits2 =
    concatBits


concatBits3 : IntW Num3 -> IntW a -> IntW (Add3 a)
concatBits3 =
    concatBits


concatBits4 : IntW Num4 -> IntW a -> IntW (Add4 a)
concatBits4 =
    concatBits


concatBits5 : IntW Num5 -> IntW a -> IntW (Add5 a)
concatBits5 =
    concatBits


concatBits6 : IntW Num6 -> IntW a -> IntW (Add6 a)
concatBits6 =
    concatBits


concatBits7 : IntW Num7 -> IntW a -> IntW (Add7 a)
concatBits7 =
    concatBits


concatBits8 : IntW Num8 -> IntW a -> IntW (Add8 a)
concatBits8 =
    concatBits


type Endianness
    = Little
    | Big


intToBits : Endianness -> IntW a -> List Bool
intToBits endianness =
    case endianness of
        Little ->
            intToBitsLittle

        Big ->
            intToBitsBig []


intToBitsLittle : IntW a -> List Bool
intToBitsLittle =
    List.Extra.unfoldr
        (\(IntW w x) ->
            if w == 0 then
                Nothing

            else
                Just
                    ( Bitwise.and 1 x == 1
                    , IntW (w - 1) (shiftRightBy 1 x)
                    )
        )


intToBitsBig : List Bool -> IntW a -> List Bool
intToBitsBig bs (IntW w x) =
    if w == 0 then
        bs

    else
        intToBitsBig ((Bitwise.and 1 x == 1) :: bs) (IntW (w - 1) (shiftRightBy 1 x))


bitsToIntF : Bool -> IntW a -> IntW a
bitsToIntF b (IntW w x) =
    IntW (w + 1)
        (Bitwise.or
            (if b then
                1

             else
                0
            )
            (shiftLeftBy 1 x)
        )


bitsToInt : Endianness -> List Bool -> IntW a
bitsToInt endianness =
    (case endianness of
        Little ->
            List.foldr

        Big ->
            List.foldl
    )
        bitsToIntF
        (IntW 0 0)


toInt : IntW a -> Int
toInt (IntW _ x) =
    x
