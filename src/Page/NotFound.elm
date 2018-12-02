module Page.NotFound exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html
import Html.Attributes
import Page exposing (Page)
import Style.Color as Color


view : { darkMode : Bool } -> Page msg
view { darkMode } =
    { title = "Not Found"
    , moreInfo = Nothing
    , body =
        Element.el
            [ Element.width Element.fill
            , Background.color (Color.contentBackground darkMode)
            , Element.height Element.fill
            ]
            (Element.el
                [ Element.centerX
                , Element.centerY
                , Font.size 24
                ]
             <|
                Element.link [ Font.color (Color.contentFont darkMode) ]
                    { url = "/", label = Element.text "Click to go back to to HomePage" }
            )
    }
