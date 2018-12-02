module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html
import Page.DataStructure as DataStructure
import Page.NotFound as NotFound
import Route exposing (Route)
import Style.Color as Color
import Task
import Url exposing (Url)
import Widget.Button as Button
import Widget.Icon as Icon


type alias Model =
    { navKey : Navigation.Key
    , windowSize : { width : Int, height : Int }
    , darkMode : Bool
    , page : PageModel
    }


type PageModel
    = DataStructure DataStructure.Model
    | NotFound


type Msg
    = NavigateTo Url
    | ClickedLink Browser.UrlRequest
    | GoToRoute Route
    | DataStructureMsg DataStructure.Msg
    | WindowSizeChanged { width : Int, height : Int }
    | ToggleDarkMode


type alias Flags =
    {}


type alias Size =
    { width : Int, height : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Navigation.load url
                    )

        GoToRoute route ->
            ( model, Navigation.pushUrl model.navKey (Route.toUrlString route) )

        NavigateTo url ->
            let
                ( page, cmd ) =
                    initPageFromUrl url
            in
            ( { model | page = page }, cmd )

        DataStructureMsg dsMsg ->
            case model.page of
                DataStructure dsModel ->
                    ( { model | page = DataStructure (DataStructure.update dsMsg dsModel) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        WindowSizeChanged windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        menuSize =
            { width = model.windowSize.width, height = 44 }

        spacing =
            3

        pageSize =
            { width = model.windowSize.width, height = model.windowSize.height - menuSize.height - spacing }

        pageView =
            case model.page of
                DataStructure dsModel ->
                    DataStructure.view
                        { toMsg = DataStructureMsg
                        , size = pageSize
                        , darkMode = model.darkMode
                        }
                        dsModel

                NotFound ->
                    NotFound.view
    in
    { title = pageView.title ++ " - Functional Data Structures"
    , body =
        [ Element.layout
            [ Font.family
                [ Font.sansSerif
                ]
            ]
          <|
            Element.column
                [ Element.width (Element.px model.windowSize.width)
                , Element.height (Element.px model.windowSize.height)
                , Element.spacing spacing
                , Background.color (Color.background model.darkMode)
                ]
                [ menuView { title = pageView.title, size = menuSize, darkMode = model.darkMode } model
                , pageView.body
                ]
        ]
    }


menuView : { title : String, size : Size, darkMode : Bool } -> Model -> Element Msg
menuView { title, size, darkMode } model =
    Element.row
        [ Element.width (Element.px size.width)
        , Element.height (Element.px size.height)
        , Background.color (Color.contentBackground model.darkMode)
        , Font.color (Color.contentFont darkMode)

        --        , Element.explain Debug.todo
        , Element.paddingXY 20 10
        ]
        [ Element.el
            [ Element.width (Element.fillPortion 1)
            , Font.size 24
            ]
            (Element.text ("Functional Data Structures - " ++ title))
        , Element.row
            [ Element.width (Element.fillPortion 1)
            , Element.spacing 10
            ]
            [ Button.plain [ Element.alignRight ]
                { onPress = ToggleDarkMode
                , label =
                    if darkMode then
                        Icon.sun

                    else
                        Icon.moon
                }
            , Button.plain [ Element.alignRight ]
                { onPress = ToggleDarkMode
                , label = Icon.github
                }
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions mode =
    Browser.Events.onResize
        (\width height ->
            WindowSizeChanged { width = width, height = height }
        )


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( page, cmd ) =
            initPageFromUrl url

        windowSizeCmd =
            Browser.Dom.getViewport
                |> Task.map
                    (\viewport ->
                        { width = round viewport.viewport.width
                        , height = round viewport.viewport.height
                        }
                    )
                |> Task.perform WindowSizeChanged
    in
    ( { navKey = navKey
      , page = page
      , windowSize = { width = 800, height = 600 }
      , darkMode = True
      }
    , Cmd.batch [ windowSizeCmd, cmd ]
    )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , onUrlChange = NavigateTo
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        }


initPageFromUrl : Url -> ( PageModel, Cmd Msg )
initPageFromUrl url =
    case Route.parseUrl url of
        Route.DataStructure ds ->
            ( DataStructure (DataStructure.init ds), Cmd.none )

        Route.NotFound ->
            ( NotFound, Cmd.none )
