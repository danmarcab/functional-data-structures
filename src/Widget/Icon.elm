module Widget.Icon exposing (checkSquare, github, info, moon, square, sun)

import Element exposing (Attribute, Element)
import FeatherIcons exposing (Icon)


github : Element msg
github =
    toElement FeatherIcons.github


sun : Element msg
sun =
    toElement FeatherIcons.sun


moon : Element msg
moon =
    toElement FeatherIcons.moon


square : Element msg
square =
    toElement FeatherIcons.square


checkSquare : Element msg
checkSquare =
    toElement FeatherIcons.checkSquare


info : Element msg
info =
    toElement FeatherIcons.info


toElement : Icon -> Element msg
toElement icon =
    FeatherIcons.toHtml [] icon |> Element.html
