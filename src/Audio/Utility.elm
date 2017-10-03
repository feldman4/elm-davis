module Audio.Utility exposing (..)

import Audio.Types exposing (..)
import Cons exposing (Cons)


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
    let
        p =
            List.length xs
    in
        (List.drop (n % p) xs) ++ (List.take (n % p) xs)


consRotate : Int -> Cons a -> Cons a
consRotate n xs =
    xs |> Cons.toList |> rotate n |> Cons.fromList |> Maybe.withDefault xs


replace : a -> a -> a -> a
replace a b c =
    if c == a then
        b
    else
        c


{-| return cumulative sum and divide by max, ie map a list of spacings
to points in [0, 1)
-}
normalizeIntervals : List Float -> List Float
normalizeIntervals xs =
    let
        n =
            List.sum xs
    in
        xs
            |> List.scanl (+) 0
            |> List.map (\x -> x / n)


intervalsToPositions : List Int -> List Float
intervalsToPositions =
    List.map toFloat >> List.map (\x -> x ^ 0.75) >> normalizeIntervals


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


{-| replace String.Extra.replace since String.Extra 1.4.0 is broken
-}
stringReplace : String -> String -> String -> String
stringReplace pattern replacement source =
    source
        |> String.split pattern
        |> List.intersperse replacement
        |> String.join ""


arc : Float -> Float -> Float -> String
arc start stop radius =
    let
        x1 =
            radius * (cos start)

        y1 =
            radius * (sin start)

        x2 =
            radius * (cos stop)

        y2 =
            radius * (sin stop)

        replace s x =
            stringReplace s (x |> toString)
    in
        """
        M x1 y1
        A rx ry x-axis-rotation large-arc-flag sweep-flag x2 y2
        """
            |> replace "rx" radius
            |> replace "ry" radius
            |> replace "x1" x1
            |> replace "y1" y1
            |> replace "x2" x2
            |> replace "y2" y2
            |> replace "x-axis-rotation" 0
            |> replace "large-arc-flag" 0
            |> replace "sweep-flag" 1
