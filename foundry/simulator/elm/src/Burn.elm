module Burn exposing
    ( Model
    , Msg
    , alertView
    , init
    , modalView
    , navbarCustomItems
    , navbarItems
    , subscriptions
    , update
    )

import Basics.Extra exposing (flip)
import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Radio as Radio
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bytes exposing (Bytes)
import Bytes.Decode
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Events
import Http
import Maybe.Extra
import Task
import Url.Builder
import WebUSB


type Status
    = Burning
    | Success Alert.Visibility
    | Failed Alert.Visibility String


type Location
    = Local
    | Server


type BurnFileSelected
    = OpenFile
    | FoundryFileSelected (Maybe File)
    | VerilogFileSelected (Maybe File)
    | BinFileSelected (Maybe File)


type BurnFile
    = BinBytes Bytes
    | DefaultFoundry
    | FoundryFile File
    | VerilogFile File
    | BinFile File


type alias Model =
    { isSupported : Bool
    , device : Maybe WebUSB.Device
    , status : Status
    , modal : Modal.Visibility
    , file : BurnFileSelected
    }


type Msg
    = MultiMsg (List Msg)
    | ShowModal Modal.Visibility
    | SetDevice (Maybe WebUSB.Device)
    | SelectDevice
    | SetFileSelected BurnFileSelected
    | SelectFile (File -> BurnFileSelected)
    | PerformBurn Location BurnFile
    | SetStatus Status


init : Bool -> Model
init isSupported =
    { isSupported = isSupported
    , device = Nothing
    , status = Success Alert.closed
    , modal = Modal.hidden
    , file = OpenFile
    }


update : (() -> List Http.Part) -> Msg -> Model -> ( Model, Cmd Msg )
update memRegFileParts msg model =
    case msg of
        MultiMsg ms ->
            Tuple.mapSecond Cmd.batch
                << List.foldl
                    (\msg2 ( model2, cmds ) ->
                        Tuple.mapSecond (flip (::) cmds) (update memRegFileParts msg2 model2)
                    )
                    ( model, [] )
            <|
                ms

        ShowModal v ->
            ( { model | modal = v }
            , Cmd.none
            )

        SetDevice device ->
            ( { model | device = device }
            , Cmd.none
            )

        SelectDevice ->
            ( model
            , WebUSB.requestDevice
            )

        SetFileSelected file ->
            ( { model | file = file }
            , Cmd.none
            )

        SelectFile advancedBurnFileSelected ->
            ( model
            , File.Select.file [ "text/plain" ] (SetFileSelected << advancedBurnFileSelected)
            )

        PerformBurn location file ->
            ( { model
                | modal = Modal.hidden
                , status = Burning
              }
            , case ( location, file ) of
                ( Local, BinBytes bs ) ->
                    Maybe.Extra.unwrap Cmd.none (WebUSB.burn model.device)
                        << Bytes.Decode.decode
                            (Bytes.Decode.loop
                                ( Bytes.width bs
                                , []
                                )
                                (\( n, xs ) ->
                                    if n == 0 then
                                        Bytes.Decode.succeed
                                            (Bytes.Decode.Done
                                                << List.reverse
                                             <|
                                                xs
                                            )

                                    else
                                        Bytes.Decode.andThen
                                            (\x ->
                                                Bytes.Decode.succeed
                                                    (Bytes.Decode.Loop
                                                        ( n - 1
                                                        , x :: xs
                                                        )
                                                    )
                                            )
                                            Bytes.Decode.unsignedInt8
                                )
                            )
                    <|
                        bs

                ( Local, BinFile f ) ->
                    Task.perform
                        (PerformBurn Local << BinBytes)
                        (File.toBytes f)

                ( _, _ ) ->
                    Http.post
                        { url =
                            Url.Builder.absolute
                                [ case location of
                                    Local ->
                                        "synth"

                                    Server ->
                                        "burn"
                                ]
                                [ Url.Builder.string "type" <|
                                    case file of
                                        BinBytes _ ->
                                            "bin"

                                        DefaultFoundry ->
                                            "defaultFoundry"

                                        FoundryFile _ ->
                                            "foundry"

                                        VerilogFile _ ->
                                            "verilog"

                                        BinFile _ ->
                                            "bin"
                                ]
                        , body =
                            let
                                memsRegs =
                                    memRegFileParts ()
                            in
                            Http.multipartBody <|
                                case file of
                                    BinBytes bs ->
                                        [ Http.bytesPart "file" "text/plain" bs ]

                                    DefaultFoundry ->
                                        memsRegs

                                    FoundryFile f ->
                                        List.concat
                                            [ [ Http.filePart "file" f ]
                                            , memsRegs
                                            ]

                                    VerilogFile f ->
                                        [ Http.filePart "file" f ]

                                    BinFile f ->
                                        [ Http.filePart "file" f ]
                        , expect =
                            Http.expectBytes
                                (\result ->
                                    let
                                        fail =
                                            SetStatus << Failed Alert.shown
                                    in
                                    case result of
                                        Ok cmd ->
                                            cmd

                                        Err (Http.BadUrl _) ->
                                            fail "Server Not Found"

                                        Err Http.Timeout ->
                                            fail "Server Timeout"

                                        Err Http.NetworkError ->
                                            fail "Network Error"

                                        Err (Http.BadStatus _) ->
                                            fail "Server Error: Invalid Status"

                                        Err (Http.BadBody _) ->
                                            fail "Server Error: Invalid Body"
                                )
                                (Bytes.Decode.andThen
                                    (\status ->
                                        case status of
                                            0 ->
                                                case location of
                                                    Local ->
                                                        Bytes.Decode.map
                                                            (PerformBurn Local << BinBytes)
                                                            (Bytes.Decode.andThen
                                                                Bytes.Decode.bytes
                                                                (Bytes.Decode.unsignedInt32 Bytes.LE)
                                                            )

                                                    Server ->
                                                        Bytes.Decode.succeed (SetStatus (Success Alert.shown))

                                            1 ->
                                                Bytes.Decode.map
                                                    (SetStatus << Failed Alert.shown)
                                                    (Bytes.Decode.andThen
                                                        Bytes.Decode.string
                                                        (Bytes.Decode.unsignedInt32 Bytes.LE)
                                                    )

                                            _ ->
                                                Bytes.Decode.succeed
                                                    (SetStatus
                                                        (Failed Alert.shown "Unknown status recieved")
                                                    )
                                    )
                                    Bytes.Decode.unsignedInt8
                                )
                        }
            )

        SetStatus status ->
            ( { model
                | status = status
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebUSB.setDevice
            SetDevice
        , WebUSB.deviceSelectionFailed
            (\e ->
                MultiMsg
                    [ SetDevice Nothing
                    , SetStatus (Failed Alert.shown e)
                    ]
            )
        , WebUSB.burnFinished
            (SetStatus
                << Maybe.Extra.unwrap
                    (Success Alert.shown)
                    (Failed Alert.shown)
            )
        , case model.status of
            Burning ->
                Sub.none

            Success v ->
                Alert.subscriptions v (SetStatus << Success)

            Failed v e ->
                Alert.subscriptions v (SetStatus << flip Failed e)
        , Modal.subscriptions model.modal ShowModal
        ]


navbarItems : (Msg -> msg) -> Maybe File -> Model -> List (Navbar.Item msg)
navbarItems msgMap openFile model =
    [ if model.isSupported then
        Navbar.itemLink
            [ Html.Events.onClick
                (msgMap
                    (PerformBurn Local
                        (Maybe.Extra.unwrap DefaultFoundry FoundryFile openFile)
                    )
                )
            ]
            [ Html.text "Local Burn" ]

      else
        Navbar.itemLink
            [ Html.Events.onClick
                (msgMap
                    (PerformBurn Server
                        (Maybe.Extra.unwrap DefaultFoundry FoundryFile openFile)
                    )
                )
            ]
            [ Html.text "Server Burn" ]
    , Navbar.itemLink
        [ Html.Events.onClick (msgMap (ShowModal Modal.shown)) ]
        [ Html.text "Advanced Burn" ]
    ]


navbarCustomItems : (Msg -> msg) -> Model -> List (Navbar.CustomItem msg)
navbarCustomItems msgMap model =
    if model.isSupported then
        [ Navbar.formItem []
            [ case model.device of
                Just dev ->
                    Badge.pillSuccess []
                        [ Html.text <| "Connected: " ++ WebUSB.deviceName dev ]

                Nothing ->
                    Badge.pillDanger []
                        [ Html.text "Disconnected" ]
            , Button.button
                [ Button.onClick (msgMap SelectDevice)
                , Button.primary
                , Button.small
                ]
                [ Html.text "Select Device" ]
            ]
        ]

    else
        []


alertView : Model -> Html Msg
alertView model =
    case model.status of
        Burning ->
            Alert.view Alert.shown
                << Alert.info
                << Alert.children [ Spinner.spinner [ Spinner.small ] [], Html.text "Burning" ]
            <|
                Alert.config

        Success visibility ->
            Alert.view visibility
                << Alert.success
                << Alert.dismissableWithAnimation (SetStatus << Success)
                << Alert.children [ Html.text "Burn succeeded" ]
            <|
                Alert.config

        Failed visibility error ->
            Alert.view visibility
                << Alert.danger
                << Alert.dismissableWithAnimation (SetStatus << flip Failed error)
                << Alert.children [ Html.text <| "Burn failed: " ++ error ]
            <|
                Alert.config


modalView : Maybe File -> Model -> Html Msg
modalView openFile model =
    Modal.view model.modal
        << Modal.withAnimation ShowModal
        << Modal.h1 [] [ Html.text "Advanced Burn" ]
        << Modal.body []
            (List.filterMap identity
                [ if model.isSupported then
                    Just <|
                        Form.group []
                            [ Form.label []
                                [ Html.text "Local Device" ]
                            , Button.button
                                [ Button.onClick SelectDevice
                                , Button.small
                                , Button.outlinePrimary
                                ]
                                [ Html.text "Select" ]
                            , Html.text
                                << Maybe.Extra.unwrap "No device selected" WebUSB.deviceName
                              <|
                                model.device
                            ]

                  else
                    Just <|
                        Badge.pillWarning []
                            [ Html.text "Local burning is only supported on Chromium based browsers" ]
                , Just
                    << Form.group []
                    << List.concat
                  <|
                    [ [ Form.label []
                            [ Html.text "File" ]
                      ]
                    , Radio.radioList "advancedBurnFileRadios"
                        [ Radio.create
                            [ Radio.checked (model.file == OpenFile)
                            , Radio.onClick (SetFileSelected OpenFile)
                            ]
                            "This File"
                        , let
                            isChecked =
                                case model.file of
                                    FoundryFileSelected _ ->
                                        True

                                    _ ->
                                        False
                          in
                          Radio.createAdvanced
                            [ Radio.checked isChecked
                            , Radio.onClick (SetFileSelected (FoundryFileSelected Nothing))
                            ]
                            (Radio.label []
                                [ Html.text "Foundry File"
                                , Button.button
                                    [ Button.onClick (SelectFile (FoundryFileSelected << Just))
                                    , Button.small
                                    , Button.outlinePrimary
                                    , Button.disabled (not isChecked)
                                    ]
                                    [ Html.text "Upload" ]
                                , Html.text <|
                                    case model.file of
                                        FoundryFileSelected (Just f) ->
                                            File.name f

                                        _ ->
                                            "No file selected"
                                ]
                            )
                        , let
                            isChecked =
                                case model.file of
                                    VerilogFileSelected _ ->
                                        True

                                    _ ->
                                        False
                          in
                          Radio.createAdvanced
                            [ Radio.checked isChecked
                            , Radio.onClick (SetFileSelected (VerilogFileSelected Nothing))
                            ]
                            (Radio.label []
                                [ Html.text "Verilog File"
                                , Button.button
                                    [ Button.onClick (SelectFile (VerilogFileSelected << Just))
                                    , Button.small
                                    , Button.outlinePrimary
                                    , Button.disabled (not isChecked)
                                    ]
                                    [ Html.text "Upload" ]
                                , Html.text <|
                                    case model.file of
                                        VerilogFileSelected (Just f) ->
                                            File.name f

                                        _ ->
                                            "No file selected"
                                ]
                            )
                        , let
                            isChecked =
                                case model.file of
                                    BinFileSelected _ ->
                                        True

                                    _ ->
                                        False
                          in
                          Radio.createAdvanced
                            [ Radio.checked isChecked
                            , Radio.onClick (SetFileSelected (BinFileSelected Nothing))
                            ]
                            (Radio.label []
                                [ Html.text "Bin File"
                                , Button.button
                                    [ Button.onClick (SelectFile (BinFileSelected << Just))
                                    , Button.small
                                    , Button.outlinePrimary
                                    , Button.disabled (not isChecked)
                                    ]
                                    [ Html.text "Upload" ]
                                , Html.text <|
                                    case model.file of
                                        BinFileSelected (Just f) ->
                                            File.name f

                                        _ ->
                                            "No file selected"
                                ]
                            )
                        ]
                    ]
                ]
            )
        << Modal.footer []
            (let
                burnFile =
                    case model.file of
                        OpenFile ->
                            Just
                                << Maybe.Extra.unwrap DefaultFoundry FoundryFile
                            <|
                                openFile

                        FoundryFileSelected f ->
                            Maybe.map FoundryFile f

                        VerilogFileSelected f ->
                            Maybe.map VerilogFile f

                        BinFileSelected f ->
                            Maybe.map BinFile f
             in
             List.filterMap identity
                [ if model.isSupported then
                    Just <|
                        Button.button
                            (case burnFile of
                                Nothing ->
                                    [ Button.primary
                                    , Button.disabled True
                                    ]

                                Just f ->
                                    [ Button.onClick (PerformBurn Local f)
                                    , Button.primary
                                    , Button.disabled False
                                    ]
                            )
                            [ Html.text "Local Burn" ]

                  else
                    Nothing
                , Just <|
                    Button.button
                        (case burnFile of
                            Nothing ->
                                [ Button.primary
                                , Button.disabled True
                                ]

                            Just f ->
                                [ Button.onClick (PerformBurn Server f)
                                , Button.primary
                                , Button.disabled False
                                ]
                        )
                        [ Html.text "Server Burn" ]
                ]
            )
    <|
        Modal.config (ShowModal Modal.hidden)
