module Style exposing (..)

import Element exposing (..)
import Element.Font as Font


title : List (Attribute msg)
title =
    [ Font.size 32
    , Font.heavy
    ]


heading : List (Attribute msg)
heading =
    [ Font.size 24
    , Font.bold
    ]


body : List (Attribute msg)
body =
    [ Font.size 16
    ]


small : List (Attribute msg)
small =
    [ Font.size 12
    ]


normalButtonColour : Color
normalButtonColour =
    rgb 1 1 1


depressedButtonColour : Color
depressedButtonColour =
    rgb 0.9 0.9 0.8
