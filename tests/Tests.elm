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
                    E.equal (NestedList.toList NestedList.empty) []
            , T.test "singleton lists" <|
                \_ ->
                    E.equal (NestedList.toList <| NestedList.one 9001) [ 9001 ]
            , T.fuzz listOfZeros "list of zeros" <|
                \list ->
                    E.equal (NestedList.toList list) []
            , T.fuzz (flatPair F.int) "list of flat values" <|
                \( nested, list ) ->
                    E.equal (NestedList.toList nested) list
            ]
        ]


listOfZeros : Fuzzer (NestedList a)
listOfZeros =
    F.map NestedList.group (F.list <| F.constant NestedList.empty)


flatPair : Fuzzer a -> Fuzzer ( NestedList a, List a )
flatPair =
    F.list
        >> F.map (\v -> ( List.map NestedList.one v, v ))
        >> F.map (Tuple.mapFirst NestedList.group)
