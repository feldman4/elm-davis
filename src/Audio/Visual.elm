module Audio.Visual exposing (..)

import Audio.Types exposing (..)
import Math.Vector3 exposing (vec3, Vec3)


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