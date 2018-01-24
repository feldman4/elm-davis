module FoldingTree exposing (..)

import Html exposing (Html, div, text, ol, ul, li)
import Html.Attributes exposing (style, hidden, value)
import Html.Events exposing (onClick)
import Array
import Array.Extra
import Parse exposing (doElm)


main_ : Html Msg
main_ =
    div []
        [ exampleTree |> viewTree draw1
        , exampleTree |> foldEveryOther False |> viewTreeConditional draw2
        , exampleTree |> unfoldedTree |> foldTreeAt [ 1, 0 ] |> viewTreeDepth draw3
        , Parse.elmExampleString |> toStringTree |> unfoldedTree |> viewTreeDepth draw3
        ]


main : Program Never (Tree ( Bool, String )) Msg
main =
    Html.beginnerProgram
        { model =
            -- exampleTree |> unfoldedTree
            Parse.elmExampleString |> toStringTree |> unfoldedTree
        , update = update
        , view = viewTreeDepth draw3
        }


type Msg
    = FoldAt (List Int)


update : Msg -> Tree ( Bool, a ) -> Tree ( Bool, a )
update (FoldAt crumbs) model =
    let
        _ =
            Debug.log "crumbs" crumbs
    in
        foldTreeAt (List.reverse crumbs |> List.drop 1) model


type Tree a
    = Node a (List (Tree a))


type alias FoldedTree a =
    Tree ( Bool, a )


draw1 : String -> Html msg
draw1 =
    (\a -> li [] [ text a ])


draw2 : ( Bool, String ) -> ( Bool, Html msg )
draw2 ( a, b ) =
    if a then
        ( True, li [ style [ ( "color", "blue" ) ] ] [ text b ] )
    else
        ( False, li [ style [ ( "color", "red" ) ] ] [ text b ] )


draw3 : Int -> List number -> ( Bool, String ) -> ( Bool, Html Msg )
draw3 numChildren crumbs ( a, b ) =
    let
        crumb =
            crumbs |> List.head |> Maybe.withDefault -1

        color_ =
            if a then
                "blue"
            else
                "black"

        prefix =
            if numChildren == 0 then
                "  "
            else if a then
                "▸ "
            else
                "▾ "
    in
        ( a
        , li
            [ style [ ( "color", color_ ), ( "font-family", "monospace" ) ]
            , onClick (FoldAt crumbs)
            ]
            [ text (prefix ++ b) ]
        )


exampleTree : Tree String
exampleTree =
    Node "top" [ Node "a" [], Node "b" [ Node "c" [] ] ]


toStringTree : String -> Tree String
toStringTree x =
    doElm x
        |> Maybe.map (parseTreeToFoldingTree "")
        |> Maybe.withDefault (Node "fuckyou" [])


parseTreeToFoldingTree : a -> Parse.Tree a -> Tree a
parseTreeToFoldingTree null tree =
    case tree of
        Parse.Branch xs ->
            Node null (List.map (parseTreeToFoldingTree null) xs)

        Parse.Leaf x ->
            Node x []


viewTree : (a -> Html msg) -> Tree a -> Html msg
viewTree f (Node a nodes) =
    ulUnstyled (f a :: List.map (viewTree f) nodes)


viewTreeConditional : (a -> ( Bool, Html msg )) -> Tree a -> Html msg
viewTreeConditional f (Node a nodes) =
    let
        ( stop, html ) =
            f a
    in
        if stop then
            ulUnstyled [ html ]
        else
            ulUnstyled (html :: List.map (viewTreeConditional f) nodes)


ulUnstyled : List (Html msg) -> Html msg
ulUnstyled =
    ul
        [ style
            [ ( "list-style-type", "none" )
            , ( "margin-left", "12px" )
            , ( "padding", "0" )
            , ( "cursor", "default" )
            , ( "user-select", "none" )
            ]
        ]


viewTreeDepth : (Int -> List Int -> a -> ( Bool, Html msg )) -> Tree a -> Html msg
viewTreeDepth f tree =
    let
        inner crumbs (Node a nodes) =
            let
                ( stop, html ) =
                    f (List.length nodes) crumbs a

                g i =
                    inner (i :: crumbs)
            in
                if stop then
                    ulUnstyled [ html ]
                else
                    ulUnstyled (html :: List.indexedMap g nodes)
    in
        inner [ 0 ] tree


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f (Node a nodes) =
    Node (f a) (List.map (mapTree f) nodes)


unfoldedTree : Tree a -> Tree ( Bool, a )
unfoldedTree =
    mapTree (\x -> ( False, x ))


copyFoldState : Tree ( Bool, a ) -> Tree ( Bool, b ) -> Tree ( Bool, b )
copyFoldState (Node ( boolA, a ) xs) (Node ( boolB, b ) ys) =
    let
        copied =
            List.map2 copyFoldState xs ys

        uncopied =
            List.drop (List.length xs) ys
    in
        (Node ( boolA, b ) (copied ++ uncopied))


foldTreeAt : List Int -> Tree ( Bool, a ) -> Tree ( Bool, a )
foldTreeAt crumbs (Node (( flag, a_ ) as a) nodes) =
    case crumbs of
        [] ->
            -- done
            if List.length nodes == 0 then
                Node ( flag, a_ ) nodes
            else
                Node ( not flag, a_ ) nodes

        n :: trail ->
            let
                nodes_ =
                    Array.fromList nodes
                        |> Array.Extra.update n (foldTreeAt trail)
                        |> Array.toList
            in
                Node a nodes_


foldEveryOther : Bool -> Tree String -> Tree ( Bool, String )
foldEveryOther this (Node a nodes) =
    let
        f i n =
            foldEveryOther (i % 2 == 1) n
    in
        Node ( this, a ) (List.indexedMap f nodes)
