module Audio.Music exposing (..)

import Audio.Types exposing (..)


noteToString : Note -> String
noteToString note =
    let
        noteSymbol =
            case note.letter of
                A ->
                    "A"

                A_ ->
                    "A#"

                B ->
                    "B"

                C ->
                    "C"

                C_ ->
                    "C#"

                D ->
                    "D"

                D_ ->
                    "D#"

                E ->
                    "E"

                F ->
                    "F"

                F_ ->
                    "F#"

                G ->
                    "G"

                G_ ->
                    "G#"
    in
        noteSymbol ++ (note.octave |> toString)


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
