module IntWidths exposing
    ( Endianness(..)
    , IntW
    , binOpW
    , bitsToInt
    , concatBits
    , fromIntW
    , intToBits
    , toIntW
    )

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import List.Extra


type IntW
    = IntW Int Int


toIntW : Int -> Int -> IntW
toIntW w =
    IntW w << modBy (2 ^ w)


fromIntW : IntW -> Int
fromIntW (IntW _ x) =
    x


binOpW : (Int -> Int -> Int) -> IntW -> IntW -> IntW
binOpW f (IntW w x) (IntW _ y) =
    IntW w (modBy (2 ^ w) (f x y))


concatBits : IntW -> IntW -> IntW
concatBits (IntW w1 x) (IntW w2 y) =
    IntW (w1 + w2) (Bitwise.or x (shiftLeftBy w1 y))


type Endianness
    = Little
    | Big


intToBits : Endianness -> IntW -> List Bool
intToBits endianness =
    case endianness of
        Little ->
            intToBitsLittle

        Big ->
            intToBitsBig []


intToBitsLittle : IntW -> List Bool
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


intToBitsBig : List Bool -> IntW -> List Bool
intToBitsBig bs (IntW w x) =
    if w == 0 then
        bs

    else
        intToBitsBig ((Bitwise.and 1 x == 1) :: bs) (IntW (w - 1) (shiftRightBy 1 x))


bitsToIntF : Bool -> IntW -> IntW
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


bitsToInt : Endianness -> List Bool -> IntW
bitsToInt endianness =
    (case endianness of
        Little ->
            List.foldr

        Big ->
            List.foldl
    )
        bitsToIntF
        (IntW 0 0)
