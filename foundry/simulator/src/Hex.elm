module Hex exposing (read, show)

import Bitwise
import Char
import List.Extra
import Maybe
import String


showNibble : Int -> Char
showNibble n =
    Maybe.withDefault '0'
        << List.Extra.getAt n
        << String.toList
    <|
        "0123456789abcdef"


showNibbles : List Char -> Int -> Int -> List Char
showNibbles cs w n =
    if w <= 0 && n == 0 then
        cs

    else
        let
            lsn =
                Bitwise.and 0x0F n
        in
        let
            rest =
                Bitwise.shiftRightZfBy 4 n
        in
        showNibbles (showNibble lsn :: cs) (w - 4) rest


show : Int -> Int -> String
show w =
    String.fromList
        << showNibbles [] w


readNibble : Char -> Maybe Int
readNibble c =
    List.Extra.elemIndex (Char.toLower c)
        << String.toList
    <|
        "0123456789abcdef"


readNibbles : List Char -> Int -> Maybe Int
readNibbles cs_ n =
    case cs_ of
        [] ->
            Just n

        c :: cs ->
            Maybe.andThen
                (readNibbles cs)
                (Maybe.map
                    (Bitwise.or (Bitwise.shiftLeftBy 4 n))
                    (readNibble c)
                )


read : String -> Maybe Int
read s =
    readNibbles (String.toList s) 0
