module Dot exposing (Dot, addEdge, addNode, empty, toString)

import Dict exposing (Dict)


type alias NodeId =
    String


type Dot
    = Dot
        { subgraphs : Dict String (List Node)
        , edges : List Edge
        }


type Node
    = Node { id : NodeId, label : String }


type Edge
    = Edge
        { from : NodeId
        , to : NodeId
        }


empty : Dot
empty =
    Dot
        { subgraphs = Dict.empty
        , edges = []
        }


addNode : { subgraph : String, id : NodeId, label : String } -> Dot -> Dot
addNode node (Dot dot) =
    let
        newSubgraphs =
            Dict.update node.subgraph
                (\maybeNodes ->
                    case maybeNodes of
                        Nothing ->
                            Just [ Node { id = node.id, label = node.label } ]

                        Just nodes ->
                            Just <| Node { id = node.id, label = node.label } :: nodes
                )
                dot.subgraphs
    in
    Dot { dot | subgraphs = newSubgraphs }


addEdge : { from : NodeId, to : NodeId } -> Dot -> Dot
addEdge edge (Dot dot) =
    Dot { dot | edges = Edge edge :: dot.edges }


toString : { darkMode : Bool } -> Dot -> String
toString { darkMode } (Dot { subgraphs, edges }) =
    let
        graph =
            if darkMode then
                [ "digraph {"
                , "graph [ranksep=0.3,bgcolor=transparent,fontcolor=gray,fontname=\"sans-serif\"];"
                , "node [shape=square,color=gray,fontcolor=gray,fontname=\"sans-serif\"];"
                , "edge [color=gray];"
                ]

            else
                [ "digraph {"
                , "graph [ranksep=0.3,bgcolor=transparent,fontcolor=darkgray,fontname=\"sans-serif\"];"
                , "node [shape=square,color=darkgray,fontcolor=darkgray,fontname=\"sans-serif\"];"
                , "edge [color=darkgray];"
                ]
    in
    (graph
        ++ subgraphsToString { darkMode = darkMode } subgraphs
        ++ List.map edgeToString edges
        ++ [ "}"
           ]
    )
        |> String.join "\n"


subgraphsToString : { darkMode : Bool } -> Dict String (List Node) -> List String
subgraphsToString { darkMode } subgraphs =
    Dict.toList subgraphs
        |> List.concatMap
            (\( subgraphId, nodes ) ->
                case nodes of
                    [] ->
                        []

                    manyNodes ->
                        [ "subgraph \"cluster_" ++ subgraphId ++ "\" {"
                        , if darkMode then
                            "color=gray;"

                          else
                            "color=darkgray"
                        , "label=\"" ++ subgraphId ++ "\""
                        ]
                            ++ List.map nodeToString manyNodes
                            ++ [ "}" ]
            )


nodeToString : Node -> String
nodeToString (Node { id, label }) =
    id ++ " [ label = \"" ++ label ++ "\" ];"


edgeToString : Edge -> String
edgeToString (Edge { from, to }) =
    from ++ " -> " ++ to ++ ";"
