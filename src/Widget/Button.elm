module Widget.Button exposing (classic, plain)

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Events as Events


plain : List (Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
plain attrs { onPress, label } =
    Element.el
        ([ Events.onClick onPress
         , Element.pointer
         ]
            ++ attrs
        )
        label


classic : List (Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
classic attrs { onPress, label } =
    Element.el
        ([ Events.onClick onPress
         , Element.pointer
         , Element.padding 5
         , Border.rounded 5
         , Border.width 2
         , Border.shadow { blur = 1, color = Element.rgb 0.5 0.5 0.5, offset = ( 1, 1 ), size = 1 }
         ]
            ++ attrs
        )
        label
