module Page.NotFound exposing (view)

import Element exposing (Element)
import Page exposing (Page)


view : Page msg
view =
    { title = "Not Found"
    , moreInfo = Nothing
    , body = Element.text "Not Found"
    }
