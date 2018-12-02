module Page.DataStructure.Stack exposing (Model, Msg, init, update, view)

import Data.Memory as Memory exposing (Memory, Pointer)
import Data.Stack as Stack exposing (Stack)
import Dict exposing (Dict)
import Dot
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes
import Json.Encode
import Page exposing (Page)
import Parser exposing ((|.), (|=), Parser)
import Set
import Style.Color as Color
import Widget.Button as Button
import Widget.Icon as Icon



-- Model


type alias Model =
    { operationText : String
    , operations : Operations
    , nextOperation : Int
    , helpOpened : Bool
    }


type alias Operations =
    Dict Int { operation : Operation, apply : Bool }


type alias Variable =
    String


type Operation
    = Assign Variable StackOperation


type StackOperation
    = Empty
    | Pop Variable
    | Push Int Variable


init : Model
init =
    { operationText = ""
    , operations = Dict.empty
    , nextOperation = 1
    , helpOpened = False
    }
        |> loadPreset Simple



-- UPDATE


type Msg
    = UpdateOperationText String
    | AddOperation
    | ToggleApplyOperation Int
    | OpenHelp
    | CloseHelp
    | LoadPreset Preset


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateOperationText str ->
            { model | operationText = str }

        ToggleApplyOperation operationId ->
            let
                toggleApply =
                    Maybe.map (\op -> { op | apply = not op.apply })
            in
            { model | operations = Dict.update operationId toggleApply model.operations }

        AddOperation ->
            case Parser.run operationParser model.operationText of
                Ok operation ->
                    addOperation operation model

                Err _ ->
                    model

        OpenHelp ->
            { model | helpOpened = True }

        CloseHelp ->
            { model | helpOpened = False }

        LoadPreset preset ->
            loadPreset preset model


addOperation : Operation -> Model -> Model
addOperation operation model =
    { model
        | operations = Dict.insert model.nextOperation { operation = operation, apply = True } model.operations
        , nextOperation = model.nextOperation + 1
    }


loadPreset : Preset -> Model -> Model
loadPreset preset model =
    loadOperations (operationsForPreset preset) model


loadOperations : List Operation -> Model -> Model
loadOperations operations model =
    let
        newOperations =
            operations
                |> List.indexedMap (\i op -> ( i, { operation = op, apply = True } ))
                |> Dict.fromList
    in
    { model | operations = newOperations, nextOperation = Dict.size newOperations + 1 }



-- VIEW


type alias ViewConfig msg =
    { darkMode : Bool
    , toMsg : Msg -> msg
    }


type alias Size =
    { width : Int, height : Int }


view :
    ViewConfig msg
    -> Model
    ->
        { operations : Size -> Element msg
        , logical : Size -> Element msg
        , shared : Size -> Element msg
        }
view config model =
    let
        appliedOperations =
            applyOperations model.operations
    in
    { operations =
        \size ->
            operationsView
                { size = size
                , toMsg = config.toMsg
                , darkMode = config.darkMode
                }
                model
    , logical =
        \size ->
            memoryDiagramView
                { size = size
                , memoryView = Memory.Logical
                , darkMode = config.darkMode
                }
                appliedOperations
    , shared =
        \size ->
            memoryDiagramView
                { size = size
                , memoryView = Memory.Shared
                , darkMode = config.darkMode
                }
                appliedOperations
    }


applyOperations :
    Operations
    ->
        { memory : Memory (Stack.Node Int)
        , stacks : Dict String (Stack Int)
        }
applyOperations operations =
    Dict.values operations
        |> List.foldl
            (\{ operation, apply } acc ->
                if apply then
                    applyOperation operation acc

                else
                    acc
            )
            { memory = Memory.empty, stacks = Dict.empty }


applyOperation :
    Operation
    ->
        { memory : Memory (Stack.Node Int)
        , stacks : Dict String (Stack Int)
        }
    ->
        { memory : Memory (Stack.Node Int)
        , stacks : Dict String (Stack Int)
        }
applyOperation operation env =
    case operation of
        Assign var Empty ->
            { env | stacks = Dict.insert var Stack.empty env.stacks }

        Assign var (Pop stack) ->
            case Dict.get stack env.stacks of
                Nothing ->
                    env

                Just existing ->
                    let
                        ( _, newStack ) =
                            Stack.pop existing env.memory
                    in
                    { env | stacks = Dict.insert var newStack env.stacks }

        Assign var (Push val stack) ->
            case Dict.get stack env.stacks of
                Nothing ->
                    env

                Just existing ->
                    let
                        ( newStack, newMemory ) =
                            Stack.push val existing env.memory
                    in
                    { env | stacks = Dict.insert var newStack env.stacks, memory = newMemory }


operationsView :
    { size : Size
    , darkMode : Bool
    , toMsg : Msg -> msg
    }
    -> Model
    -> Element msg
operationsView config model =
    let
        operationView id apply operation =
            Element.row [ Element.spacing 10 ]
                [ Button.plain []
                    { onPress = config.toMsg <| ToggleApplyOperation id
                    , label =
                        if apply then
                            Icon.checkSquare

                        else
                            Icon.square
                    }
                , Element.el
                    [ Font.family [ Font.monospace ]
                    , Font.bold
                    ]
                  <|
                    Element.text (operationToString operation)
                ]

        addOperationView =
            Element.column [ Element.spacing 5 ]
                [ Input.text
                    [ Background.color (Color.contentBackground config.darkMode)
                    , Border.color (Color.contentFont config.darkMode)
                    , Border.width 2
                    , Font.color (Color.contentFont config.darkMode)
                    , Font.family [ Font.monospace ]
                    , Font.bold
                    , Element.padding 5
                    ]
                    { label = Input.labelAbove [ Element.alignLeft ] (Element.text "Add an operation")
                    , onChange = config.toMsg << UpdateOperationText
                    , placeholder = Just <| Input.placeholder [] (Element.text "E.g. myVar = empty")
                    , text = model.operationText
                    }
                , Element.row [ Element.width Element.fill ]
                    [ --                    Button.classic [ Element.alignLeft ] { onPress = config.toMsg OpenHelp, label = Element.text "Help" }
                      case Parser.run operationParser model.operationText of
                        Ok operation ->
                            Button.classic [ Element.alignRight ] { onPress = config.toMsg AddOperation, label = Element.text "Add" }

                        Err _ ->
                            Element.none
                    ]
                ]

        loadPresetView =
            Element.column [ Element.spacing 5 ]
                [ Element.text "Load a preset"
                , Element.row
                    [ Element.spaceEvenly
                    , Element.width Element.fill
                    ]
                    [ Button.classic [] { onPress = config.toMsg (LoadPreset Simple), label = Element.text "Simple" }
                    ]
                ]
    in
    Element.column
        [ Element.padding 10
        , Element.spacing 20
        , Element.scrollbarY
        ]
        [ Element.column
            [ Element.spacing 5 ]
            (List.map
                (\( id, { operation, apply } ) ->
                    operationView id apply operation
                )
                (Dict.toList model.operations)
            )
        , addOperationView
        , loadPresetView
        ]


operationToString : Operation -> String
operationToString operation =
    case operation of
        Assign var Empty ->
            var ++ " = empty"

        Assign var (Pop stack) ->
            var ++ " = pop " ++ stack

        Assign var (Push val stack) ->
            var ++ " = push " ++ String.fromInt val ++ " " ++ stack


memoryDiagramView :
    { size : Size
    , memoryView : Memory.View
    , darkMode : Bool
    }
    ->
        { memory : Memory (Stack.Node Int)
        , stacks : Dict String (Stack Int)
        }
    -> Element msg
memoryDiagramView { size, memoryView, darkMode } { memory, stacks } =
    let
        dot =
            Memory.toDot memoryView
                { deref = Stack.deref
                , toLabel = \node -> String.fromInt node.element
                , variables =
                    stacks
                        |> Dict.toList
                        |> List.map
                            (Tuple.mapSecond
                                (\stack ->
                                    case stack of
                                        Stack.Empty ->
                                            Nothing

                                        Stack.Top pointer ->
                                            Just pointer
                                )
                            )
                }
                memory

        dotString =
            Dot.toString { darkMode = darkMode } dot
    in
    Element.el
        [ Element.centerX
        , Element.width (Element.px size.width)
        , Element.height (Element.px size.height)
        ]
    <|
        Element.html <|
            Html.node "dot-render"
                [ Html.Attributes.property "content" (Json.Encode.string dotString)
                , Html.Attributes.property "width" (Json.Encode.int size.width)
                , Html.Attributes.property "height" (Json.Encode.int size.height)
                ]
                []



-- PARSER


operationParser : Parser Operation
operationParser =
    Parser.succeed Assign
        |= variableParser
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= stackOperationParser


stackOperationParser : Parser StackOperation
stackOperationParser =
    Parser.oneOf
        [ Parser.succeed Empty
            |. Parser.keyword "empty"
        , Parser.succeed Pop
            |. Parser.keyword "pop"
            |. Parser.spaces
            |= variableParser
        , Parser.succeed Push
            |. Parser.keyword "push"
            |. Parser.spaces
            |= Parser.int
            |. Parser.spaces
            |= variableParser
        ]


variableParser : Parser Variable
variableParser =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isLower
        , reserved = Set.fromList [ "empty", "pop", "push" ]
        }



-- PRESETS OF OPERATIONS


type Preset
    = Simple
    | Complex


operationsForPreset : Preset -> List Operation
operationsForPreset preset =
    case preset of
        Simple ->
            [ Assign "a" Empty
            , Assign "a" (Push 1 "a")
            , Assign "a" (Push 2 "a")
            , Assign "a" (Push 3 "a")
            , Assign "b" (Push 4 "a")
            , Assign "b" (Push 5 "b")
            , Assign "c" (Push 5 "a")
            ]

        Complex ->
            []
