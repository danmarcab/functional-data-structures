module Data.Memory exposing
    ( Address
    , Memory
    , Pointer(..)
    , View(..)
    , delete
    , empty
    , get
    , insert
    , size
    , toDebugList
    , toDot
    , toLogicalDot
    , toSharedDot
    , update
    )

import Dict exposing (Dict)
import Dot exposing (Dot)


type Memory a
    = Memory
        { nextAddress : Address
        , contents : Dict Address a
        }


type View
    = Shared
    | Logical


type alias Address =
    Int


type Pointer a
    = Pointer Address


empty : Memory a
empty =
    Memory
        { nextAddress = 1
        , contents = Dict.empty
        }


get : Pointer a -> Memory a -> Maybe a
get (Pointer address) (Memory memory) =
    Dict.get address memory.contents


size : Memory a -> Int
size (Memory memory) =
    Dict.size memory.contents


insert : a -> Memory a -> ( Memory a, Pointer a )
insert a (Memory memory) =
    ( Memory { nextAddress = memory.nextAddress + 1, contents = Dict.insert memory.nextAddress a memory.contents }
    , Pointer memory.nextAddress
    )


update : (a -> a) -> Pointer a -> Memory a -> Memory a
update fun (Pointer address) (Memory memory) =
    Memory { memory | contents = Dict.update address (Maybe.map fun) memory.contents }


delete : Pointer a -> Memory a -> Memory a
delete (Pointer address) (Memory memory) =
    Memory { memory | contents = Dict.remove address memory.contents }


toDot :
    View
    ->
        { deref : a -> List (Pointer a)
        , toLabel : a -> String
        , variables : List ( String, Maybe (Pointer a) )
        }
    -> Memory a
    -> Dot
toDot view data memory =
    case view of
        Shared ->
            toSharedDot data memory

        Logical ->
            toLogicalDot data memory


toSharedDot :
    { deref : a -> List (Pointer a)
    , toLabel : a -> String
    , variables : List ( String, Maybe (Pointer a) )
    }
    -> Memory a
    -> Dot
toSharedDot { deref, toLabel, variables } memory =
    let
        heap =
            toDebugList deref memory
                |> List.foldl
                    (\{ address, content, pointers } dot ->
                        List.foldl
                            (\pointer currentDot ->
                                Dot.addEdge { from = String.fromInt address, to = String.fromInt pointer } currentDot
                            )
                            (Dot.addNode { subgraph = "memory", id = String.fromInt address, label = toLabel content } dot)
                            pointers
                    )
                    Dot.empty
    in
    List.foldl
        (\( variableName, maybePointer ) currentDot ->
            currentDot
                |> Dot.addNode { subgraph = "variables", id = variableName, label = variableName }
                |> (\dotWithVariable ->
                        case maybePointer of
                            Nothing ->
                                dotWithVariable

                            Just (Pointer address) ->
                                Dot.addEdge { from = variableName, to = String.fromInt address } dotWithVariable
                   )
        )
        heap
        variables


toLogicalDot :
    { deref : a -> List (Pointer a)
    , toLabel : a -> String
    , variables : List ( String, Maybe (Pointer a) )
    }
    -> Memory a
    -> Dot
toLogicalDot { deref, toLabel, variables } (Memory memory) =
    let
        id name address =
            name ++ String.fromInt address

        expand name address dot =
            Dict.get address memory.contents
                |> Maybe.map
                    (\node ->
                        List.foldl
                            (\(Pointer add) currentDot ->
                                currentDot
                                    |> Dot.addEdge { from = id name address, to = id name add }
                                    |> expand name add
                            )
                            (Dot.addNode { subgraph = "memory", id = id name address, label = toLabel node } dot)
                            (deref node)
                    )
                |> Maybe.withDefault dot
    in
    List.foldl
        (\( variableName, maybePointer ) currentDot ->
            currentDot
                |> Dot.addNode { subgraph = "variables", id = variableName, label = variableName }
                |> (\dotWithVariable ->
                        case maybePointer of
                            Nothing ->
                                dotWithVariable

                            Just (Pointer address) ->
                                Dot.addEdge { from = variableName, to = variableName ++ String.fromInt address } dotWithVariable
                                    |> (\dot ->
                                            expand variableName address dot
                                       )
                   )
        )
        Dot.empty
        variables


toDebugList :
    (a -> List (Pointer a))
    -> Memory a
    -> List { address : Address, content : a, pointers : List Address }
toDebugList deref (Memory memory) =
    Dict.map
        (\address node ->
            { address = address, content = node, pointers = deref node |> List.map (\(Pointer add) -> add) }
        )
        memory.contents
        |> Dict.values
