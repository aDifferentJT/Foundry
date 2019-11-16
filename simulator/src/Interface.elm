module Interface exposing (InspectibleMem, Interface, Sim, TickRes, interface)

import Array exposing (Array)
import Array.Extra
import Basics.Extra exposing (flip)
import Bitwise
import Browser exposing (Document, document)
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List exposing (repeat)
import List.Extra
import Maybe exposing (withDefault)
import Maybe.Extra
import String exposing (fromFloat)
import Style
import Task
import Time


type alias InspectibleMem simState =
    { title : String
    , contents :
        List
            { value : String
            , set : String -> Maybe simState
            , selected : Bool
            }
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


type alias Model simState =
    { simState : simState
    , running : Bool
    , clockSpeed : Float
    , buttonsPressed : Array Bool
    , editingMems : Dict ( Int, Int ) String
    , debugMsg : String
    , width : Int
    , height : Int
    }


type Msg simState
    = ManyMsgs (List (Msg simState))
    | TwoMsgs (Msg simState) (Msg simState)
    | ButtonPress Int
    | ButtonRelease Int
    | Tick
    | ChangeClockSpeed Float
    | ChangeState simState
    | StartEditingMem Int Int String
    | StopEditingMem Int Int
    | DidResize Int Int


type alias Interface simState =
    Program () (Model simState) (Msg simState)


interface : Sim simState -> Interface simState
interface sim =
    Browser.document
        { init = init sim
        , update = update sim
        , subscriptions = subscriptions sim
        , view = view sim
        }


init : Sim simState -> () -> ( Model simState, Cmd (Msg simState) )
init sim _ =
    ( { simState = sim.defaultState
      , running = False
      , clockSpeed = 125
      , buttonsPressed = Array.repeat 16 False
      , editingMems = Dict.empty
      , debugMsg = ""
      , width = 100
      , height = 100
      }
    , Task.perform
        (\viewport ->
            DidResize (round viewport.scene.width) (round viewport.scene.height)
        )
        Browser.Dom.getViewport
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
            ( (if n == sim.executeButton then
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
            , Cmd.none
            )

        ButtonRelease n ->
            ( { model
                | buttonsPressed = Array.set n False model.buttonsPressed
              }
            , Cmd.none
            )

        Tick ->
            ( performTick sim model
            , Cmd.none
            )

        ChangeClockSpeed x ->
            ( { model
                | clockSpeed = x
              }
            , Cmd.none
            )

        ChangeState s ->
            ( { model
                | simState = s
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

        DidResize w h ->
            ( { model
                | width = w
                , height = h
              }
            , Cmd.none
            )


subscriptions : Sim simState -> Model simState -> Sub (Msg simState)
subscriptions sim model =
    Sub.batch
        [ if model.running then
            Time.every model.clockSpeed (always Tick)

          else
            Sub.none
        , Browser.Events.onResize DidResize
        ]


type alias Table simState =
    List (List (Element (Msg simState)))


ledColumns : List (Element.Column { a | ledRecord : Array (Element (Msg simState)) } (Msg simState))
ledColumns =
    List.map
        (\n ->
            { header = Element.none
            , width = Element.shrink
            , view = \record -> withDefault Element.none << Array.get n <| record.ledRecord
            }
        )
    <|
        List.range 0 7


buttonColumns : List (Element.Column { a | buttonRecord : Array (Element (Msg simState)) } (Msg simState))
buttonColumns =
    List.map
        (\n ->
            { header = Element.none
            , width = Element.shrink
            , view = \record -> withDefault Element.none << Array.get n <| record.buttonRecord
            }
        )
    <|
        List.range 0 2


ledTable : Sim simState -> Model simState -> List (Array (Element (Msg simState)))
ledTable sim model =
    List.map
        (Array.fromList
            << List.map
                (\led ->
                    Element.el []
                        << Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Element.width << Element.px <| 10
                            , Element.height << Element.px <| 10
                            , Element.Border.rounded << Bitwise.shiftRightZfBy 1 <| -1
                            , Element.Background.color <|
                                if led then
                                    Element.rgb 1 0 0

                                else
                                    Element.rgb 0 0 0
                            , Element.Border.glow (Element.rgb 1 0 0) <|
                                if led then
                                    5

                                else
                                    0
                            ]
                    <|
                        Element.none
                )
        )
        << List.Extra.greedyGroupsOf 8
        << List.reverse
        << sim.getLeds
    <|
        model.simState



{-
   onTouchStart : Msg simState -> Html.Attribute (Msg simState)
   onTouchStart msg =
       Html.Events.on "touchstart" (Decode.succeed msg)


   onTouchEnd : Msg simState -> Html.Attribute (Msg simState)
   onTouchEnd msg =
       Html.Events.on "touchend" (Decode.succeed msg)


   onTouchLeave : Msg simState -> Html.Attribute (Msg simState)
   onTouchLeave msg =
       Html.Events.on "touchleave" (Decode.succeed msg)
-}


makeButton : Sim simState -> Model simState -> Int -> Element (Msg simState)
makeButton sim model n =
    let
        pressed =
            withDefault False << Array.get n <| model.buttonsPressed
    in
    Element.Input.button
        [ Element.Events.onMouseDown (ButtonPress n)
        , Element.Events.onMouseUp (ButtonRelease n)
        , Element.Events.onMouseLeave (ButtonRelease n)
        ]
        { label =
            Element.el
                ([ Element.padding 5
                 , Element.Border.width 1
                 , Element.Border.rounded
                    << Bitwise.shiftRightZfBy 1
                   <|
                    -1
                 , Element.Background.color <|
                    if withDefault False << Array.get n <| model.buttonsPressed then
                        Style.depressedButtonColour

                    else
                        Style.normalButtonColour
                 ]
                    ++ Style.small
                )
                << Element.text
                << sim.getButtonName
            <|
                n
        , onPress = Nothing
        }


bottomButtonTable : Sim simState -> Model simState -> List (Array (Element (Msg simState)))
bottomButtonTable sim model =
    [ Array.fromList
        << List.map (makeButton sim model)
        << List.reverse
      <|
        List.range 0 7
    ]


sideButtonTable : Sim simState -> Model simState -> List (Array (Element (Msg simState)))
sideButtonTable sim model =
    List.map
        (Array.fromList
            << List.map (makeButton sim model)
        )
        << List.Extra.greedyGroupsOf 2
        << List.reverse
    <|
        List.range 8 15


memTable : Sim simState -> Model simState -> Element (Msg simState)
memTable sim model =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        ]
        << List.indexedMap
            (\memI mem ->
                Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                <|
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        [ Element.el Style.heading << Element.text <| mem.title
                        , Element.indexedTable
                            [ Element.spacing 10
                            , Element.padding 10
                            , Element.width Element.fill
                            , Element.height Element.fill
                            , Element.scrollbarY
                            ]
                            { data = mem.contents
                            , columns =
                                [ { header = Element.none
                                  , width = Element.shrink
                                  , view =
                                        \_ cell ->
                                            Element.el
                                                [ Element.alignRight
                                                , Element.centerY
                                                ]
                                                << Element.text
                                            <|
                                                if cell.selected then
                                                    "->"

                                                else
                                                    ""
                                  }
                                , { header = Element.none
                                  , width = Element.shrink
                                  , view =
                                        \n _ ->
                                            Element.el
                                                [ Element.Font.center
                                                , Element.centerY
                                                , Element.Font.variant Element.Font.tabularNumbers
                                                ]
                                                << Element.text
                                                << String.fromInt
                                            <|
                                                n
                                  }
                                , { header = Element.none
                                  , width = Element.fill
                                  , view =
                                        \n cell ->
                                            Element.el
                                                [ Element.width << Element.px <| 500
                                                ]
                                                << Element.Input.text
                                                    [ Element.Background.color
                                                        (if Dict.member ( memI, n ) model.editingMems then
                                                            Element.rgb 1 0 0

                                                         else
                                                            Element.rgb 1 1 1
                                                        )
                                                    ]
                                            <|
                                                { onChange =
                                                    \s ->
                                                        Maybe.Extra.unwrap
                                                            (StartEditingMem memI n s)
                                                            (TwoMsgs (StopEditingMem memI n) << ChangeState)
                                                            << cell.set
                                                        <|
                                                            s
                                                , text =
                                                    withDefault cell.value
                                                        << Dict.get ( memI, n )
                                                    <|
                                                        model.editingMems
                                                , placeholder = Nothing
                                                , label = Element.Input.labelHidden <| mem.title ++ " " ++ String.fromInt n
                                                }
                                  }
                                ]
                            }
                        ]
            )
        << sim.getInspectibleMems
    <|
        model.simState


view : Sim simState -> Model simState -> Document (Msg simState)
view sim model =
    { title = "Foundry Simulator"
    , body =
        [ Element.layout
            ([ Element.width << Element.px <| model.width
             , Element.height << Element.px <| model.height
             ]
                ++ Style.body
            )
            << Element.column
                [ Element.spacing 20
                , Element.paddingEach
                    { top = 20
                    , bottom = 0
                    , left = 20
                    , right = 20
                    }
                , Element.height Element.fill
                , Element.scrollbarY
                , Element.centerX
                ]
          <|
            [ Element.el Style.title << Element.text <| "Foundry Simulator"
            , Element.Input.slider
                [ Element.height (Element.px 30)
                , Element.width (Element.px 100)

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Element.Background.color <| Element.rgb 0.5 0.5 0.5
                        , Element.Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = ChangeClockSpeed
                , label = Element.Input.labelLeft [] << Element.text <| "Clock speed: " ++ fromFloat model.clockSpeed
                , min = 100
                , max = 999
                , value = model.clockSpeed
                , thumb = Element.Input.defaultThumb
                , step = Just 1
                }
            , Element.table
                [ Element.spacing 10
                , Element.padding 10
                ]
                { data =
                    List.map2
                        (\ledRecord buttonRecord ->
                            { ledRecord = ledRecord
                            , buttonRecord = buttonRecord
                            }
                        )
                        (ledTable sim model
                            ++ bottomButtonTable sim model
                        )
                        (sideButtonTable sim model)
                , columns = ledColumns ++ buttonColumns
                }
            , Element.text <| "Debug: " ++ model.debugMsg
            , memTable sim model
            ]
        ]
    }
