module Tests exposing (suite)

import Test as T exposing (Test)
import Fuzz as F exposing (Fuzzer)
import Expect as E
import NestedList exposing (NestedList)


suite : Test
suite =
    T.describe "Nested List"
        [ T.describe "toList"
            [ T.test "empty lists" <|
                \_ ->
                    E.equal (NestedList.toList NestedList.zero) []
            , T.test "singleton lists" <|
                \_ ->
                    E.equal (NestedList.toList <| NestedList.one 9001) [ 9001 ]
            , T.fuzz listOfZeros "list of zeros" <|
                \list ->
                    E.equal (NestedList.toList list) []
            , T.fuzz (flatPair F.int) "list of flat values" <|
                \( nested, list ) ->
                    E.equal (NestedList.toList nested) list
            , T.test "nested lists values" <|
                \_ ->
                    let
                        input =
                            NestedList.many
                                [ NestedList.zero
                                , NestedList.one 9001
                                , NestedList.many []
                                , NestedList.many
                                    [ NestedList.zero
                                    , NestedList.one 9001
                                    , NestedList.many []
                                    , NestedList.many
                                        [ NestedList.zero
                                        , NestedList.one 9001
                                        ]
                                    ]
                                ]
                    in
                        E.equal (NestedList.toList input) [ 9001, 9001, 9001 ]
            ]
        ]


listOfZeros : Fuzzer (NestedList a)
listOfZeros =
    F.map NestedList.many (F.list <| F.constant NestedList.zero)


flatPair : Fuzzer a -> Fuzzer ( NestedList a, List a )
flatPair =
    F.list
        >> F.map (\v -> ( List.map NestedList.one v, v ))
        >> F.map (Tuple.mapFirst NestedList.many)
