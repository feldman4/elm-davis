module TestNative exposing (..)

import Html exposing (text)
import NativeModule


type AType
    = A
    | B Int


f : AType -> String
f x =
    case x of
        A ->
            "A"

        B y ->
            "B"


g : AType -> String
g =
    NativeModule.memoize f


main : Html.Html msg
main =
    let
        a =
            g (B 3)

        b =
            g (B 3)
    in
        text ("asdf")
