module List.Pad exposing (padLeft, padRight)

import Basics.Extra exposing (curry, uncurry)


type Either a b
    = Left a
    | Right b


padLeft_ : a -> Int -> List a -> Either (List a) Int
padLeft_ p n xs =
    if n == 0 then
        Right << List.length <| xs

    else
        case padLeft_ p (n - 1) xs of
            Left ys ->
                Left (p :: ys)

            Right 0 ->
                Left (p :: xs)

            Right m ->
                Right (m - 1)


padLeft : a -> Int -> List a -> List a
padLeft p n xs =
    case padLeft_ p n xs of
        Left ys ->
            ys

        Right m ->
            List.drop m xs


padRight : a -> Int -> List a -> List a
padRight p n xs =
    if n == 0 then
        []

    else
        case xs of
            [] ->
                List.repeat n p

            y :: ys ->
                y :: padRight p (n - 1) ys
