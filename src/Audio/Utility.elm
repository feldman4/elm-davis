module Audio.Utility exposing (..)

import Audio.Types exposing (..)
import Cons exposing (Cons)
import List.Extra


strEq : a -> a -> Bool
strEq a a_ =
    (toString a) == (toString a_)


{-| order matters
-}
permutations : Int -> List a -> List (List a)
permutations k xs =
    let
        iter ( a, bs ) =
            permutations (k - 1) bs |> List.map ((::) a)
    in
        case k of
            1 ->
                xs |> List.map (\x -> [ x ])

            _ ->
                List.Extra.select xs |> List.concatMap iter


sign : comparable -> number
sign x =
    if x >= 0 then
        1
    else
        -1


{-| itertools.product
-}
product : List (List a) -> List (List a)
product xss =
    case xss of
        xs :: rest ->
            product rest
                |> List.concatMap (\ys -> List.map (\x -> x :: ys) xs)

        [] ->
            [ [] ]


{-| order doesn't matter.
-}
combinations : Int -> List a -> List (List a)
combinations k xs =
    let
        iter ( _, a, bs ) =
            combinations (k - 1) bs |> List.map ((::) a)
    in
        case k of
            1 ->
                xs |> List.map (\x -> [ x ])

            _ ->
                List.Extra.selectSplit xs
                    |> List.concatMap iter


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


printFullNote : FullNote -> String
printFullNote note =
    (note.letter |> printLetter) ++ (note.octave |> toString)


printLetter : Letter -> String
printLetter letter =
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


circleClamp : Int -> Int -> Int -> Int
circleClamp low high x =
    (x - low) % (high - low) + low
