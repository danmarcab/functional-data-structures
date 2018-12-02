module Page exposing (Page)

import Element exposing (Element)


type alias Page msg =
    { title : String
    , moreInfo : Maybe String
    , body : Element msg
    }
