module Data.Stack exposing (..)

import Data.Memory as Memory exposing (Memory, Pointer)


type Stack a
    = Empty
    | Top (Memory.Pointer (Node a))


type alias Node a =
    { element : a
    , tail : Stack a
    }


empty : Stack a
empty =
    Empty


push : a -> Stack a -> Memory (Node a) -> ( Stack a, Memory (Node a) )
push a stack memory =
    let
        newNode =
            { element = a
            , tail = stack
            }

        ( newMemory, pointerToNode ) =
            Memory.insert newNode memory
    in
    ( Top pointerToNode, newMemory )


pushMany : List a -> Stack a -> Memory (Node a) -> ( Stack a, Memory (Node a) )
pushMany elements initialStack memory =
    List.foldl
        (\element ( stack, mem ) ->
            push element stack mem
        )
        ( initialStack, memory )
        elements


pop : Stack a -> Memory (Node a) -> ( Maybe a, Stack a )
pop stack memory =
    case stack of
        Empty ->
            ( Nothing, stack )

        Top pointer ->
            Memory.get pointer memory
                |> Maybe.map (\node -> ( Just node.element, node.tail ))
                |> Maybe.withDefault ( Nothing, stack )


peek : Stack a -> Memory (Node a) -> Maybe a
peek stack memory =
    case stack of
        Empty ->
            Nothing

        Top pointer ->
            Memory.get pointer memory
                |> Maybe.map .element


fromList : List a -> Memory (Node a) -> ( Stack a, Memory (Node a) )
fromList elements memory =
    pushMany elements Empty memory


toList : Stack a -> Memory (Node a) -> List a
toList stack memory =
    case stack of
        Empty ->
            []

        Top pointer ->
            Memory.get pointer memory
                |> Maybe.map
                    (\node ->
                        node.element :: toList node.tail memory
                    )
                |> Maybe.withDefault []



-- DEREF


deref : Node a -> List (Pointer (Node a))
deref node =
    case node.tail of
        Empty ->
            []

        Top pointer ->
            [ pointer ]
