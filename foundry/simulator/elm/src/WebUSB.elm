port module WebUSB exposing
    ( Device
    , burn
    , burnFinished
    , deviceName
    , deviceSelectionFailed
    , requestDevice
    , setDevice
    )

import Json.Decode


type alias Device =
    ( Json.Decode.Value, String )


deviceName : Device -> String
deviceName =
    Tuple.second


port portRequestDevice : () -> Cmd msg


requestDevice : Cmd msg
requestDevice =
    portRequestDevice ()


port setDevice : (Maybe Device -> msg) -> Sub msg


port deviceSelectionFailed : (String -> msg) -> Sub msg


port portBurn : ( Maybe Json.Decode.Value, List Int ) -> Cmd msg


burn : Maybe Device -> List Int -> Cmd msg
burn dev bs =
    portBurn
        ( Maybe.map Tuple.first dev
        , bs
        )


port burnFinished : (Maybe String -> msg) -> Sub msg
