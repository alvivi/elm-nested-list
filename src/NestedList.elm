module NestedList
    exposing
        ( NestedList
        , zero
        , one
        , many
        , group
        , fromMany
        , toList
        , ifTrue
        , ifFalse
        , maybe
        , maybeMap
        )

{-| A list of values of any type, which can also be lists.

The main use case of this library is managing list of optional and nested
values, for example, like `Html` nodes or `Html.Attribute`s. Usually, we can
solve this issue using `List (List any)` or `List (Maybe any)`, like this:

    myView : Bool -> Html msg
    myView isHidden =
        Html.div []
            (List.concat
                [ [ Html.text "Always visible"
                  , Html.text "Also always visible"
                  ]
                , if isHidden then
                    []
                  else
                    [ Html.text "Maybe visible" ]
                , if not isHidden then
                    [ Html.text "Maybe not visible" ]
                  else
                    []
                ]
            )

Using `NestedList`, the above code will look like this:

    myView : Bool -> Html msg
    myView isHidden =
        Html.div []
            (NestedList.fromMany
                [ NestedList.one (Html.text "Always visible")
                , NestedList.one (Html.text "Always visible")
                , NestedList.ifTrue isHidden (Html.text "Maybe visible")
                , NestedList.ifFalse isHidden (Html.text "Maybe not visible")
                , NestedList many
                    [ Html.text "Even nested list, without calling again fromMany"
                    ]
                ]
            )


## A Note About Performance

This library optimizes converting `NestedList` back to a list in linear
time. Here is a benchmark comparing using `NestedList` against using `List` and
`List.concat`.

![Benchmark](https://raw.githubusercontent.com/alvivi/elm-nested-list/master/assets/elm-nested-list-benchmark.png)


# Creating Nested Lists

@docs NestedList, zero, one, manyj group


# Conversion to Lists

@docs fromMany, toList


# Helpful Functions

@docs ifTrue, ifFalse, maybe, maybeMap

-}

-- Creating Nested List --


{-| A list of values or nested values.
-}
type NestedList a
    = Zero
    | One a
    | Many (List a)
    | Group (List (NestedList a))


{-| Returns an empty `NestedList a`, **O(1)**. The implementation uses a special
case for empty list in order to have similar performance than a
`List (Maybe a)`.
-}
zero : NestedList a
zero =
    Zero


{-| Returns a singleton `NestedList a`, i.e. a list with only one element,
**O(1)**.
-}
one : a -> NestedList a
one =
    One


{-| Transform a `List a` of values into `NestedList a` value, **O(1)**.
-}
many : List a -> NestedList a
many =
    Many


{-| Groups a `List (NestedList a)` of values into `NestedList a` value,
**O(1)**.
-}
group : List (NestedList a) -> NestedList a
group =
    Group



-- Conversion to Lists --


{-| Converts a `List` of `NestedList` into a `List`, **O(n)**. Same than
`NestedList.many >> NestedList.toList`.
-}
fromMany : List (NestedList a) -> List a
fromMany =
    Group >> toList


{-| Converts a `NestedList` into a `List a`, **O(n)**.
-}
toList : NestedList a -> List a
toList list =
    case list of
        Zero ->
            []

        One value ->
            [ value ]

        Many list ->
            list

        Group [] ->
            []

        Group (Zero :: tail) ->
            toList (Group tail)

        Group ((One value) :: tail) ->
            value :: toList (Group tail)

        Group ((Many []) :: tail) ->
            toList (Group tail)

        Group ((Many flatTail) :: tail) ->
            flatTail ++ toList (Group tail)

        Group ((Group []) :: tail) ->
            toList (Group tail)

        Group ((Group (Zero :: more)) :: tail) ->
            (toList (Group (Group more :: tail)))

        Group ((Group ((One value) :: more)) :: tail) ->
            value :: (toList (Group (Group more :: tail)))

        Group ((Group ((Many flatList) :: more)) :: tail) ->
            flatList ++ toList (Group (Group more :: tail))

        Group ((Group ((Group more) :: evenMore)) :: tail) ->
            toList (Group (Group more :: Group evenMore :: tail))



-- Helpful Functions --


{-| Given a predicate, return an empty `NestedList` if the predicates is true,
or singleton `NestedList` otherwise. **O(1)**.
-}
ifTrue : Bool -> a -> NestedList a
ifTrue pred value =
    if pred then
        One value
    else
        Zero


{-| Given a predicate, return an empty `NestedList` if the predicates is false,
or singleton `NestedList` if it is true. **O(1)**.
-}
ifFalse : Bool -> a -> NestedList a
ifFalse pred value =
    if not pred then
        One value
    else
        Zero


{-| Converts a `Maybe a` into a singleton `NestedList`, **O(1)**.
-}
maybe : Maybe a -> NestedList a
maybe maybeValue =
    case maybeValue of
        Nothing ->
            Zero

        Just value ->
            One value


{-| Transform a `Maybe` value given a function and then transforms that `Maybe`
into a `NestedList`, **O(1)**. Like `Maybe.map fn >> maybe`.
-}
maybeMap : (b -> a) -> Maybe b -> NestedList a
maybeMap fn =
    Maybe.map fn >> maybe
