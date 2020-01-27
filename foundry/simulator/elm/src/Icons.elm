module Icons exposing
    ( downloadAsmIcon
    , downloadHexIcon
    , svgIcons
    , uploadAsmIcon
    , uploadHexIcon
    )

import Basics.Extra exposing (flip)
import Color
import Html exposing (Html)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Core
import TypedSvg.Types


use : String -> TypedSvg.Core.Svg msg
use =
    flip TypedSvg.use [] << List.singleton << TypedSvg.Attributes.xlinkHref


decoration : String -> String -> TypedSvg.Core.Svg msg
decoration id t =
    TypedSvg.symbol
        [ TypedSvg.Attributes.id id
        , TypedSvg.Attributes.viewBox 0 0 200 100
        ]
        [ TypedSvg.text_
            [ TypedSvg.Attributes.x (TypedSvg.Types.px 70)
            , TypedSvg.Attributes.y (TypedSvg.Types.px 40)
            , TypedSvg.Attributes.style "font-size: 48"
            ]
            [ TypedSvg.Core.text t
            ]
        ]


svgIcons : Html msg
svgIcons =
    TypedSvg.svg [ TypedSvg.Attributes.display TypedSvg.Types.DisplayNone ]
        [ TypedSvg.symbol
            [ TypedSvg.Attributes.id "download-upload-base"
            , TypedSvg.Attributes.viewBox 0 0 200 100
            ]
            [ TypedSvg.path
                [ TypedSvg.Attributes.d "M 5 80 v 15 h 90 v -15"
                , TypedSvg.Attributes.stroke Color.black
                , TypedSvg.Attributes.strokeWidth (TypedSvg.Types.px 10)
                , TypedSvg.Attributes.noFill
                ]
                []
            , TypedSvg.line
                [ TypedSvg.Attributes.x1 (TypedSvg.Types.px 50)
                , TypedSvg.Attributes.y1 (TypedSvg.Types.px 10)
                , TypedSvg.Attributes.x2 (TypedSvg.Types.px 50)
                , TypedSvg.Attributes.y2 (TypedSvg.Types.px 70)
                , TypedSvg.Attributes.stroke Color.black
                , TypedSvg.Attributes.strokeWidth (TypedSvg.Types.px 10)
                ]
                []
            ]
        , TypedSvg.symbol
            [ TypedSvg.Attributes.id "download-icon"
            , TypedSvg.Attributes.viewBox 0 0 200 100
            ]
            [ use "#download-upload-base"
            , TypedSvg.path
                [ TypedSvg.Attributes.d "M 50 80 l -20 -20 h 40 l -20 20"
                , TypedSvg.Attributes.fill (TypedSvg.Types.Fill Color.black)
                ]
                []
            ]
        , TypedSvg.symbol
            [ TypedSvg.Attributes.id "upload-icon"
            , TypedSvg.Attributes.viewBox 0 0 200 100
            ]
            [ use "#download-upload-base"
            , TypedSvg.path
                [ TypedSvg.Attributes.d "M 50 0 l -20 20 h 40 l -20 -20"
                , TypedSvg.Attributes.fill (TypedSvg.Types.Fill Color.black)
                ]
                []
            ]
        , decoration "asm-decoration" "asm"
        , decoration "hex-decoration" "0x"
        ]


icon : List String -> Html msg
icon =
    TypedSvg.svg
        [ TypedSvg.Attributes.width (TypedSvg.Types.em 4)
        , TypedSvg.Attributes.height (TypedSvg.Types.em 2)
        ]
        << List.map use


downloadAsmIcon : Html msg
downloadAsmIcon =
    icon
        [ "#download-icon"
        , "#asm-decoration"
        ]


uploadAsmIcon : Html msg
uploadAsmIcon =
    icon
        [ "#upload-icon"
        , "#asm-decoration"
        ]


downloadHexIcon : Html msg
downloadHexIcon =
    icon
        [ "#download-icon"
        , "#hex-decoration"
        ]


uploadHexIcon : Html msg
uploadHexIcon =
    icon
        [ "#upload-icon"
        , "#hex-decoration"
        ]
