port module WebUSB exposing (burn, burnFinished)


port burn : List Int -> Cmd msg


port burnFinished : (Maybe String -> msg) -> Sub msg
