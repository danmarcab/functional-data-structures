module Style.Color exposing
    ( background
    , contentBackground
    , contentFont
    , titleBackground
    , titleFont
    )

import Element exposing (Color)


background : Bool -> Color
background darkMode =
    if darkMode then
        Element.rgb 0.1 0.1 0.1

    else
        Element.rgb 0.4 0.4 0.4


titleBackground : Bool -> Color
titleBackground darkMode =
    if darkMode then
        Element.rgb 0.3 0.3 0.3

    else
        Element.rgb 0.7 0.7 0.7


titleFont : Bool -> Color
titleFont darkMode =
    if darkMode then
        Element.rgb 0.9 0.9 0.9

    else
        Element.rgb 0.1 0.1 0.1


contentBackground : Bool -> Color
contentBackground darkMode =
    if darkMode then
        Element.rgb 0.2 0.2 0.2

    else
        Element.rgb 0.8 0.8 0.8


contentFont : Bool -> Color
contentFont darkMode =
    if darkMode then
        Element.rgb 0.9 0.9 0.9

    else
        Element.rgb 0.1 0.1 0.1
