module Interface exposing
    ( InspectibleMem
    , Interface
    , Sim
    , TickRes
    , errorPage
    , interface
    )

import Array exposing (Array)
import Basics.Extra exposing (flip, uncurry)
import Bitwise
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Range as Range
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Size as Size
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Hex
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Icons
import Json.Decode
import List.Extra
import List.Pad
import Maybe exposing (withDefault)
import Maybe.Extra
import Task
import Time
import Url.Builder


type alias InspectibleMem simState =
    { name : String
    , contents :
        List
            { value : String
            , set : String -> Maybe simState
            , getHex : ( Int, Int )
            , selected : Bool
            }
    , setAll : List String -> simState
    , setAllHex : List Int -> simState
    }


type alias TickRes simState =
    { debugMsg : String
    , newState : simState
    }


type alias Sim simState =
    { defaultState : simState
    , performButton : Int -> simState -> simState
    , getButtonName : Int -> String
    , runButton : Int
    , executeButton : Int
    , tick : simState -> TickRes simState
    , getLeds : simState -> List Bool
    , getInspectibleMems : simState -> List (InspectibleMem simState)
    }


type BurnStatus
    = Burning
    | BurnSuccess Alert.Visibility
    | BurnFailed String Alert.Visibility


type alias Model simState =
    { navbarState : Navbar.State
    , simState : simState
    , running : Bool
    , clockSpeed : Float
    , buttonsPressed : Array Bool
    , editingMems : Dict ( Int, Int ) String
    , debugMsg : String
    , burnStatus : BurnStatus
    }


type MemoryFileFormat
    = HumanReadable
    | Hexadecimal


type Msg simState
    = NOP
    | NavbarMsg Navbar.State
    | ManyMsgs (List (Msg simState))
    | TwoMsgs (Msg simState) (Msg simState)
    | ButtonPress Int
    | ButtonRelease Int
    | Tick
    | ChangeClockSpeed Float
    | ChangeState simState
    | StartEditingMem Int Int String
    | StopEditingMem Int Int
    | DownloadMem MemoryFileFormat (InspectibleMem simState)
    | OfferUploadMem MemoryFileFormat (InspectibleMem simState)
    | PerformUploadMem MemoryFileFormat (InspectibleMem simState) File
    | CompleteUploadMem MemoryFileFormat (InspectibleMem simState) String
    | Burn
    | SetBurnStatus BurnStatus
    | OfferOpenFile
    | PerformOpenFile File
    | CompleteOpenFile String


type alias Interface simState =
    Program () (Model simState) (Msg simState)


interface : Sim simState -> Interface simState
interface sim =
    Browser.document
        { init = init sim
        , update = update sim
        , subscriptions = subscriptions sim
        , view = view (Ok sim)
        }


errorPage : String -> Interface ()
errorPage e =
    let
        sim =
            { defaultState = ()
            , performButton = \_ () -> ()
            , getButtonName = \_ -> ""
            , runButton = 0
            , executeButton = 0
            , tick = \() -> { debugMsg = "", newState = () }
            , getLeds = \() -> []
            , getInspectibleMems = \() -> []
            }
    in
    Browser.document
        { init = init sim
        , update = update sim
        , subscriptions = subscriptions sim
        , view = view (Err e)
        }


init : Sim simState -> () -> ( Model simState, Cmd (Msg simState) )
init sim _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState
      , simState = sim.defaultState
      , running = False
      , clockSpeed = 200
      , buttonsPressed = Array.repeat 16 False
      , editingMems = Dict.empty
      , debugMsg = ""
      , burnStatus = BurnSuccess Alert.closed
      }
    , navbarCmd
    )


updateMemScroll : Sim simState -> Model simState -> ( Model simState, Cmd (Msg simState) )
updateMemScroll sim model =
    ( model
    , Cmd.batch
        << List.map
            (\mem ->
                case Maybe.map Tuple.first << List.head << List.filter (.selected << Tuple.second) << List.indexedMap Tuple.pair <| mem.contents of
                    Nothing ->
                        Cmd.none

                    Just cell ->
                        Task.attempt (\_ -> NOP)
                            (Task.andThen
                                (\( memViewport, memElement, cellElement ) ->
                                    Browser.Dom.setViewportOf
                                        ("mem_" ++ mem.name)
                                        0
                                        (memViewport.viewport.y
                                            + (cellElement.element.y - memElement.element.y)
                                            + ((cellElement.element.height - memElement.element.height) / 2)
                                        )
                                )
                                (Task.map3 (\x y z -> ( x, y, z ))
                                    (Browser.Dom.getViewportOf ("mem_" ++ mem.name))
                                    (Browser.Dom.getElement ("mem_" ++ mem.name))
                                    (Browser.Dom.getElement ("mem_" ++ mem.name ++ "_cell_" ++ String.fromInt cell))
                                )
                            )
            )
        << sim.getInspectibleMems
      <|
        model.simState
    )


performTick : Sim simState -> Model simState -> Model simState
performTick sim model =
    let
        tickRes =
            sim.tick model.simState
    in
    { model
        | simState = tickRes.newState
        , debugMsg = tickRes.debugMsg
    }


update : Sim simState -> Msg simState -> Model simState -> ( Model simState, Cmd (Msg simState) )
update sim msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        NavbarMsg navbarState ->
            ( { model | navbarState = navbarState }, Cmd.none )

        ManyMsgs ms ->
            Tuple.mapSecond Cmd.batch
                << List.foldr
                    (\msg_ ( model_, cs ) ->
                        Tuple.mapSecond (flip (::) cs) << update sim msg_ <| model_
                    )
                    ( model, [] )
            <|
                ms

        TwoMsgs m1 m2 ->
            update sim (ManyMsgs [ m1, m2 ]) model

        ButtonPress n ->
            updateMemScroll sim
                << (if n == sim.executeButton then
                        performTick sim

                    else
                        identity
                   )
            <|
                { model
                    | simState = sim.performButton n model.simState
                    , running = xor model.running (n == sim.runButton)
                    , buttonsPressed = Array.set n True model.buttonsPressed
                }

        ButtonRelease n ->
            ( { model
                | buttonsPressed = Array.set n False model.buttonsPressed
              }
            , Cmd.none
            )

        Tick ->
            updateMemScroll sim << performTick sim <| model

        ChangeClockSpeed clockSpeed ->
            ( { model
                | clockSpeed = clockSpeed
              }
            , Cmd.none
            )

        ChangeState simState ->
            ( { model
                | simState = simState
              }
            , Cmd.none
            )

        StartEditingMem i j s ->
            ( { model
                | editingMems = Dict.insert ( i, j ) s model.editingMems
              }
            , Cmd.none
            )

        StopEditingMem i j ->
            ( { model
                | editingMems = Dict.remove ( i, j ) model.editingMems
              }
            , Cmd.none
            )

        DownloadMem format mem ->
            ( model
            , File.Download.string
                (mem.name ++ ".txt")
                "text/plain"
                (String.join "\n"
                    << List.map
                        (case format of
                            HumanReadable ->
                                .value

                            Hexadecimal ->
                                uncurry Hex.show << .getHex
                        )
                 <|
                    mem.contents
                )
            )

        OfferUploadMem format mem ->
            ( model
            , File.Select.file [ "text/plain" ] (PerformUploadMem format mem)
            )

        PerformUploadMem format mem file ->
            ( model
            , Task.perform (CompleteUploadMem format mem)
                << File.toString
              <|
                file
            )

        CompleteUploadMem format mem string ->
            ( { model
                | simState =
                    (case format of
                        HumanReadable ->
                            mem.setAll

                        Hexadecimal ->
                            mem.setAllHex << List.map (withDefault 0 << Hex.read)
                    )
                        << List.Pad.padRight "" (List.length mem.contents)
                        << String.split "\n"
                    <|
                        string
                , editingMems = Dict.empty
              }
            , Cmd.none
            )

        Burn ->
            ( { model
                | burnStatus = Burning
              }
            , Http.post
                { url = "/burn"
                , body = Http.emptyBody
                , expect =
                    Http.expectJson
                        (SetBurnStatus
                            << (|>) Alert.shown
                            << (\result ->
                                    case result of
                                        Ok burnStatus ->
                                            burnStatus

                                        Err (Http.BadUrl _) ->
                                            BurnFailed "Server Not Found"

                                        Err Http.Timeout ->
                                            BurnFailed "Server Timeout"

                                        Err Http.NetworkError ->
                                            BurnFailed "Network Error"

                                        Err (Http.BadStatus _) ->
                                            BurnFailed "Server Error: Invalid Status"

                                        Err (Http.BadBody _) ->
                                            BurnFailed "Server Error: Invalid Body"
                               )
                        )
                        (Json.Decode.oneOf
                            [ Json.Decode.null BurnSuccess
                            , Json.Decode.map BurnFailed Json.Decode.string
                            ]
                        )
                }
            )

        SetBurnStatus burnStatus ->
            ( { model
                | burnStatus = burnStatus
              }
            , Cmd.none
            )

        OfferOpenFile ->
            ( model
            , File.Select.file [ "text/plain" ] PerformOpenFile
            )

        PerformOpenFile f ->
            ( model
            , Task.perform CompleteOpenFile << File.toString <| f
            )

        CompleteOpenFile s ->
            ( model
            , Browser.Navigation.load <| Url.Builder.absolute [ "open" ] [ Url.Builder.string "file" s ]
            )


subscriptions : Sim simState -> Model simState -> Sub (Msg simState)
subscriptions sim model =
    Sub.batch
        << List.filterMap identity
    <|
        [ if model.running then
            Just <| Time.every model.clockSpeed (always Tick)

          else
            Nothing
        , case model.burnStatus of
            Burning ->
                Nothing

            BurnSuccess v ->
                Just <| Alert.subscriptions v (SetBurnStatus << BurnSuccess)

            BurnFailed e v ->
                Just <| Alert.subscriptions v (SetBurnStatus << BurnFailed e)
        ]


type alias Table simState =
    List (List (Html (Msg simState)))


ledTable : Sim simState -> Model simState -> List (List (Html msg))
ledTable sim model =
    List.map
        (List.map
            (\led ->
                Html.span
                    [ Html.Attributes.style "display" "inline-block"
                    , Html.Attributes.style "width" "10px"
                    , Html.Attributes.style "height" "10px"
                    , Html.Attributes.style "background-color"
                        (if led then
                            "red"

                         else
                            "black"
                        )
                    , Html.Attributes.style "box-shadow"
                        (if led then
                            "0px 0px 10px 5px red"

                         else
                            "0px 0px 0px 0px red"
                        )
                    , Html.Attributes.style "transition-property" "background-color, box-shadow"
                    , Html.Attributes.style "transition-duration" "0.1s"
                    , Html.Attributes.style "transition-timing-function" "ease-in-out"
                    , Html.Attributes.style "animation-name"
                        (if led then
                            "activate_led"

                         else
                            "deactivate_led"
                        )
                    , Html.Attributes.style "animation-duration" "1s"
                    , Border.circle
                    ]
                    [ Html.text "\u{00A0}" ]
            )
        )
        << List.Extra.greedyGroupsOf 8
        << List.reverse
        << sim.getLeds
    <|
        model.simState


makeButton : Sim simState -> Model simState -> Int -> Html (Msg simState)
makeButton sim model n =
    let
        pressed =
            withDefault False << Array.get n <| model.buttonsPressed
    in
    Button.button
        [ Button.onClick (ButtonPress n)

        --, Button.onMouseUp (ButtonRelease n)
        --, Button.onMouseLeave (ButtonRelease n)
        , Button.light
        , Button.outlineDark
        , Button.attrs
            [ Border.all
            , Border.rounded

            {-
               , Element.Background.color <|
                  if withDefault False << Array.get n <| model.buttonsPressed then
                      Style.depressedButtonColour

                  else
                      Style.normalButtonColour
            -}
            ]
        ]
        [ Html.text (sim.getButtonName n) ]


bottomButtonTable : Sim simState -> Model simState -> List (List (Html (Msg simState)))
bottomButtonTable sim model =
    [ List.map (makeButton sim model)
        << List.reverse
      <|
        List.range 0 7
    ]


sideButtonTable : Sim simState -> Model simState -> List (List (Html (Msg simState)))
sideButtonTable sim model =
    List.map (List.map (makeButton sim model))
        << List.Extra.greedyGroupsOf 2
        << List.reverse
    <|
        List.range 8 15


memTable : Sim simState -> Model simState -> Html (Msg simState)
memTable sim model =
    Grid.row
        [ Row.attrs
            [ Html.Attributes.class "h-100"
            ]
        ]
        << List.indexedMap
            (\memI mem ->
                Grid.col
                    [ Col.attrs
                        [ Html.Attributes.class "h-100"
                        , Html.Attributes.class "d-flex"
                        , Html.Attributes.class "flex-column"
                        ]
                    ]
                    [ Grid.row []
                        [ Grid.col
                            [ Col.attrs
                                [ Html.Attributes.class "col-md-auto"
                                ]
                            ]
                            [ Html.h2 []
                                [ Html.text <| mem.name ++ ":"
                                ]
                            ]
                        , Grid.col
                            [ Col.attrs
                                [ Html.Attributes.class "col-md-auto"
                                ]
                            ]
                            [ Button.button
                                [ Button.onClick (DownloadMem HumanReadable mem)
                                , Button.attrs [ Html.Attributes.style "padding" "0px" ]
                                ]
                                [ Icons.downloadAsmIcon ]
                            ]
                        , Grid.col
                            [ Col.attrs
                                [ Html.Attributes.class "col-md-auto"
                                ]
                            ]
                            [ Button.button
                                [ Button.onClick (OfferUploadMem HumanReadable mem)
                                , Button.attrs [ Html.Attributes.style "padding" "0px" ]
                                ]
                                [ Icons.uploadAsmIcon ]
                            ]
                        , Grid.col
                            [ Col.attrs
                                [ Html.Attributes.class "col-md-auto"
                                ]
                            ]
                            [ Button.button
                                [ Button.onClick (DownloadMem Hexadecimal mem)
                                , Button.attrs [ Html.Attributes.style "padding" "0px" ]
                                ]
                                [ Icons.downloadHexIcon ]
                            ]
                        , Grid.col
                            [ Col.attrs
                                [ Html.Attributes.class "col-md-auto"
                                ]
                            ]
                            [ Button.button
                                [ Button.onClick (OfferUploadMem Hexadecimal mem)
                                , Button.attrs [ Html.Attributes.style "padding" "0px" ]
                                ]
                                [ Icons.uploadHexIcon ]
                            ]
                        ]
                    , Grid.row
                        [ Row.attrs
                            [ Html.Attributes.id <| "mem_" ++ mem.name
                            , Html.Attributes.class "h-100"
                            , Html.Attributes.style "overflow-y" "auto"
                            ]
                        ]
                        [ Grid.col [ Col.attrs [ Html.Attributes.class "h-100" ] ]
                            [ Table.table
                                { options = []
                                , thead = Table.thead [] [ Table.tr [] [] ]
                                , tbody =
                                    Table.tbody []
                                        << List.indexedMap
                                            (\n cell ->
                                                Table.tr
                                                    (List.filterMap identity
                                                        [ Just
                                                            << Table.rowAttr
                                                            << Html.Attributes.id
                                                          <|
                                                            "mem_"
                                                                ++ mem.name
                                                                ++ "_cell_"
                                                                ++ String.fromInt n
                                                        , if cell.selected then
                                                            Just Table.rowActive

                                                          else
                                                            Nothing
                                                        ]
                                                    )
                                                    [ Table.td [] [ Html.text (String.fromInt n) ]
                                                    , Table.td []
                                                        [ Input.text
                                                            << List.filterMap identity
                                                          <|
                                                            [ Just
                                                                << Input.value
                                                                << withDefault cell.value
                                                                << Dict.get ( memI, n )
                                                              <|
                                                                model.editingMems
                                                            , Just
                                                                << Input.onInput
                                                              <|
                                                                \s ->
                                                                    Maybe.Extra.unwrap
                                                                        (StartEditingMem memI n s)
                                                                        (TwoMsgs (StopEditingMem memI n) << ChangeState)
                                                                        (cell.set s)
                                                            , if Dict.member ( memI, n ) model.editingMems then
                                                                Just Input.danger

                                                              else
                                                                Nothing
                                                            ]
                                                        ]
                                                    ]
                                            )
                                    <|
                                        mem.contents
                                }
                            ]
                        ]
                    ]
            )
        << sim.getInspectibleMems
    <|
        model.simState


view : Result String (Sim simState) -> Model simState -> Browser.Document (Msg simState)
view simRes model =
    { title = "Foundry Simulator"
    , body =
        [ Bootstrap.CDN.stylesheet
        , Icons.svgIcons
        , Html.div
            [ Html.Attributes.class "d-flex"
            , Html.Attributes.class "flex-column"
            , Html.Attributes.style "height" "100vh"
            ]
            [ Navbar.view model.navbarState
                << Navbar.withAnimation
                << Navbar.brand [] [ Html.h1 [] [ Html.text "Foundry Simulator" ] ]
                << Navbar.items
                    [ Navbar.itemLink
                        [ Html.Events.onClick OfferOpenFile ]
                        [ Html.text "Open" ]
                    ]
                << Navbar.customItems
                    (case simRes of
                        Err _ ->
                            []

                        Ok _ ->
                            [ Navbar.formItem []
                                [ Html.text <| "Clock speed (ms): " ++ String.fromFloat model.clockSpeed
                                , Range.range
                                    [ Range.min "100"
                                    , Range.max "999"
                                    , Range.value << String.fromFloat <| model.clockSpeed
                                    , Range.step "1"
                                    , Range.onInput (Just (ChangeClockSpeed << withDefault 200 << String.toFloat))
                                    ]
                                ]
                            , Navbar.formItem
                                []
                                [ Button.button
                                    [ Button.onClick Burn
                                    ]
                                    [ Html.text "Burn" ]
                                ]
                            ]
                    )
              <|
                Navbar.config NavbarMsg
            , case model.burnStatus of
                Burning ->
                    Alert.view Alert.shown
                        << Alert.info
                        << Alert.children [ Spinner.spinner [ Spinner.small ] [], Html.text "Burning" ]
                    <|
                        Alert.config

                BurnSuccess visibility ->
                    Alert.view visibility
                        << Alert.success
                        << Alert.dismissableWithAnimation (SetBurnStatus << BurnSuccess)
                        << Alert.children [ Html.text "Burn succeeded" ]
                    <|
                        Alert.config

                BurnFailed error visibility ->
                    Alert.view visibility
                        << Alert.danger
                        << Alert.dismissableWithAnimation (SetBurnStatus << BurnFailed error)
                        << Alert.children [ Html.text <| "Burn failed: " ++ error ]
                    <|
                        Alert.config
            , case simRes of
                Err e ->
                    Html.pre [] [ Html.text e ]

                Ok sim ->
                    Grid.container
                        [ Html.Attributes.class "h-100"
                        , Html.Attributes.class "d-flex"
                        , Html.Attributes.class "flex-column"
                        ]
                        [ Grid.row []
                            [ Grid.col []
                                [ Table.table
                                    { options = []
                                    , thead = Table.thead [] [ Table.tr [] [] ]
                                    , tbody =
                                        Table.tbody []
                                            << List.map (Table.tr [])
                                            << List.map
                                                (List.map
                                                    (Table.td
                                                        [ Table.cellAttr (Html.Attributes.align "center")
                                                        , Table.cellAttr (Html.Attributes.attribute "valign" "middle")
                                                        ]
                                                        << List.singleton
                                                    )
                                                )
                                        <|
                                            List.map2 (++)
                                                (ledTable sim model
                                                    ++ bottomButtonTable sim model
                                                )
                                                (sideButtonTable sim model)
                                    }
                                ]
                            ]
                        , Grid.row [] [ Grid.col [] [ Html.text <| "Debug: " ++ model.debugMsg ] ]
                        , Grid.row
                            [ Row.attrs
                                [ Html.Attributes.class "h-100"
                                ]
                            ]
                            [ Grid.col
                                [ Col.attrs
                                    [ Html.Attributes.class "h-100"
                                    ]
                                ]
                                [ memTable sim model ]
                            ]
                        ]
            ]
        ]
    }
