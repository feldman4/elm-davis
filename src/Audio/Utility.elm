module Audio.Utility exposing (..)

import Audio.Types exposing (..)


type Cons2 a
    = Cons2 a a (List a)


cons2map : (a -> b) -> Cons2 a -> Cons2 b
cons2map f (Cons2 x1 x2 xs) =
    Cons2 (f x1) (f x2) (List.map f xs)


cons2fromList : List a -> Maybe (Cons2 a)
cons2fromList xs =
    case xs of
        x1 :: x2 :: rest ->
            Just (Cons2 x1 x2 rest)

        _ ->
            Nothing


rotate : Int -> List a -> List a
rotate n xs =
    (List.drop n xs) ++ (List.take n xs)


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


noteToString : Note -> String
noteToString note =
    (note.letter |> letterToString) ++ (note.octave |> toString)


letterToString : Letter -> String
letterToString letter =
    case letter of
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


intToNote : Int -> Note
intToNote intNote =
    let
        letters =
            [ C, C_, D, D_, E, F, F_, G, G_, A, A_, B ]
    in
        { letter =
            letters
                |> List.drop (intNote % 12)
                |> List.head
                |> Maybe.withDefault A
        , octave = (floor ((intNote |> toFloat) / 12)) - 1
        }


noteToInt : Note -> Int
noteToInt note =
    note.letter |> letterToPosition |> (+) ((note.octave + 1) * 12)


relativeToPresent : Float -> NoteEvent -> NoteEvent
relativeToPresent time noteEvent =
    let
        fromPresent x =
            time - x
    in
        { noteEvent
            | start = fromPresent noteEvent.start
            , end = noteEvent.end |> Maybe.map fromPresent
        }
