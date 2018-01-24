module Food exposing (..)

import Html exposing (Html, text, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Html Message
main =
    ul []
        [ li [ style [ ( "color", "blue" ) ], onClick (ClickEvent 0) ]
            [ recipe |> List.map printFood |> String.join ", " |> text ]
        , li [ onClick KeyEvent ] [ recipeString |> String.join ", " |> text ]
        ]


type Message
    = ClickEvent Int
    | KeyEvent


x : List String
x =
    recipe |> List.map printFood


recipeString : List String
recipeString =
    [ Banana |> toString
    , Apple |> toString
    , Knives |> toString
    ]


recipe : List Food
recipe =
    [ Banana, Apple ]


printFood : Food -> String
printFood food =
    case food of
        Banana ->
            "banana"

        Apple ->
            "apple"

        _ ->
            "I don't know"


type Food
    = Banana
    | Apple
    | Orange


type DinnerWare
    = Plates
    | Knives
