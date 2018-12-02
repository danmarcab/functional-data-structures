module Route exposing (Route(..), parseUrl, toUrlString)

import Page.DataStructure exposing (DataStructure(..))
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = DataStructure DataStructure
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map (DataStructure Stack) Parser.top
        , Parser.map (DataStructure Stack) (Parser.s "stack")
        , Parser.map NotFound (Parser.s "not-found")
        ]


toUrlString : Route -> String
toUrlString route =
    case route of
        DataStructure Stack ->
            Url.Builder.absolute [ "stack" ] []

        NotFound ->
            Url.Builder.absolute [ "not-found" ] []


parseUrl : Url -> Route
parseUrl url =
    Parser.parse routeParser url
        |> Maybe.withDefault NotFound
