module Types exposing (..)

import Html exposing (..)


main : Html msg
main =
    text ""


type alias Note =
    { letter : Letter, octave : Int }


type Letter
    = A
    | A_
    | B
    | C
    | C_
    | D
    | D_
    | E
    | F
    | F_
    | G
    | G_
