module Music exposing (..)

import Html exposing (..)
import Types exposing (..)
import Math.Vector3 exposing (vec3, Vec3)


main : Html msg
main =
    text ""


letterToColor : Letter -> Vec3
letterToColor letter =
    case letter of
        A ->
            vec3 0.5 0 0

        A_ ->
            vec3 0 1 0.5

        B ->
            vec3 0 0.5 0

        C ->
            vec3 0 0 0.5

        C_ ->
            vec3 0.5 0 0.5

        D ->
            vec3 0 0.5 0.5

        D_ ->
            vec3 0.5 0.5 0

        E ->
            vec3 0.1 0.2 0.3

        F ->
            vec3 0.3 0.2 0.1

        F_ ->
            vec3 0.9 0.1 0.1

        G ->
            vec3 0.6 0.2 0.4

        G_ ->
            vec3 0.2 0.8 0.2



-- _ ->
--     vec3 0.5 0.5 0.5


letterToPosition : Letter -> Int
letterToPosition letter =
    case letter of
        C ->
            0

        C_ ->
            1

        D ->
            2

        D_ ->
            3

        E ->
            4

        F ->
            5

        F_ ->
            6

        G ->
            7

        G_ ->
            8

        A ->
            9

        A_ ->
            10

        B ->
            11
