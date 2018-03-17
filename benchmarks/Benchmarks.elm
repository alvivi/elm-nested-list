module Benchmarks exposing (main)

import Benchmark as B exposing (Benchmark)
import Benchmark.Runner as B exposing (BenchmarkProgram)
import NestedList as N exposing (NestedList)


suite : Benchmark
suite =
    let
        size =
            512

        value =
            9001

        flatNestedListSample =
            value
                |> List.repeat size
                |> List.map N.one
                |> N.group

        flatListOfListsSample =
            value
                |> List.repeat size
                |> List.map List.singleton

        heterogeneousNestedListSample =
            heterogeneousNestedListStep
                |> List.repeat size
                |> List.map N.one
                |> N.group

        heterogeneousListOfListsSample =
            heterogeneousListOfListsStep
                |> List.repeat size
                |> List.map List.singleton

        heterogeneousNestedListStep =
            N.group
                [ N.zero
                , N.one value
                , N.many [ value, value ]
                , N.many [ value, value, value ]
                , N.many [ value, value ]
                , N.one value
                , N.zero
                ]

        heterogeneousListOfListsStep =
            [ []
            , [ value ]
            , [ value, value ]
            , [ value, value, value ]
            , [ value, value ]
            , [ value ]
            , []
            ]
    in
        B.describe "Nested List"
            [ B.describe "Converting Back to a List"
                [ B.compare "Flat Data"
                    "Nested List"
                    (\_ -> N.toList heterogeneousNestedListSample)
                    "List of Lists"
                    (\_ -> List.concat heterogeneousListOfListsSample)
                , B.compare "Heterogeneous Data"
                    "Nested List"
                    (\_ -> N.toList heterogeneousNestedListSample)
                    "List of Lists"
                    (\_ -> List.concat heterogeneousListOfListsSample)
                ]
            ]


main : BenchmarkProgram
main =
    B.program suite
