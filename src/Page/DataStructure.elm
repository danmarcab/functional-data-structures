module Page.DataStructure exposing (DataStructure(..), Model, Msg, init, update, view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Page exposing (Page)
import Page.DataStructure.Stack as Stack
import Style.Color as Color
import Widget.Button as Button


type alias Model =
    { dataStructure : DataStructureModel }


type DataStructure
    = Stack


type DataStructureModel
    = StackModel Stack.Model


type Msg
    = StackMsg Stack.Msg


type alias Size =
    { width : Int, height : Int }


init : DataStructure -> Model
init ds =
    case ds of
        Stack ->
            { dataStructure = StackModel Stack.init }


dataStructureToString : DataStructureModel -> String
dataStructureToString ds =
    case ds of
        StackModel _ ->
            "Stack"


type alias Config msg =
    { toMsg : Msg -> msg
    }


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.dataStructure ) of
        ( StackMsg stackMsg, StackModel stackModel ) ->
            { model | dataStructure = StackModel (Stack.update stackMsg stackModel) }



-- VIEW


type alias ViewConfig msg =
    { toMsg : Msg -> msg
    , darkMode : Bool
    , size : Size
    }


view : ViewConfig msg -> Model -> Page msg
view config model =
    { title =
        case model.dataStructure of
            StackModel _ ->
                "Stack"
    , moreInfo =
        case model.dataStructure of
            StackModel _ ->
                Just "More info about stacks"
    , body = view3cols config model
    }


view3cols : ViewConfig msg -> Model -> Element msg
view3cols config model =
    let
        operationsWidth =
            230

        spacing =
            3

        oddCorrection =
            if modBy 2 config.size.width == 1 then
                1

            else
                0

        memoryWidth =
            (config.size.width - operationsWidth - 2 * spacing) // 2

        dataStructureViews =
            case model.dataStructure of
                StackModel stackModel ->
                    Stack.view { darkMode = config.darkMode, toMsg = config.toMsg << StackMsg } stackModel
    in
    Element.row
        [ Element.spacing spacing
        ]
        [ windowLikeView
            { size = { width = memoryWidth + oddCorrection, height = config.size.height }
            , title = "Logical View"
            , darkMode = config.darkMode
            }
            dataStructureViews.logical
        , windowLikeView
            { size = { width = operationsWidth, height = config.size.height }
            , title = "Operations"
            , darkMode = config.darkMode
            }
            dataStructureViews.operations
        , windowLikeView
            { size = { width = memoryWidth + oddCorrection, height = config.size.height }
            , title = "Shared View"
            , darkMode = config.darkMode
            }
            dataStructureViews.shared
        ]


windowLikeView : { size : Size, title : String, darkMode : Bool } -> (Size -> Element msg) -> Element msg
windowLikeView { size, title, darkMode } content =
    let
        titleHeight =
            20
    in
    Element.column
        [ Element.width (Element.px size.width)
        , Element.height (Element.px size.height)
        ]
        [ Element.el
            [ Element.padding 2
            , Element.width (Element.px size.width)
            , Element.height (Element.px titleHeight)
            , Background.color (Color.titleBackground darkMode)
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
            , Font.center
            , Font.size 16
            , Font.bold
            , Font.color (Color.titleFont darkMode)
            ]
            (Element.text title)
        , Element.el
            [ Element.centerX
            , Element.alignTop
            , Element.width (Element.px size.width)
            , Element.height (Element.px (size.height - titleHeight))
            , Font.center
            , Font.size 16
            , Font.color (Color.titleFont darkMode)
            , Background.color (Color.contentBackground darkMode)
            ]
            (content { width = size.width, height = size.height - titleHeight })
        ]
