module Audio.Fretted exposing (..)

import Html
import Audio.Music exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Cons
import Dict


main : Html.Html msg
main =
    Html.text ""


example : String
example =
    dMajor7
        |> (\c -> { c | root = c.root + 4 })
        |> allInversionsWindow 5
        |> List.concatMap (chordToGrips standard)
        |> List.filter (\xs -> xs |> sixToList |> List.filterMap identity |> List.length |> (==) 4)
        |> List.map printGrip
        |> List.sort
        |> String.join "\n"


printGrip : Six (Maybe Int) -> String
printGrip =
    let
        f : Maybe Int -> String
        f =
            Maybe.map toString >> Maybe.withDefault "x"
    in
        sixToList >> List.map f >> String.join "."


{-|
1. List frets vs notes
-}
chordToGrips : Tuning -> Rooted Chord -> List Grip
chordToGrips tuning chord =
    let
        fret note open =
            let
                x =
                    note - open
            in
                if (0 <= x) && (x <= 24) then
                    Just x
                else
                    Nothing

        openNotes =
            tuning |> sixToList

        voices =
            chord.intervals |> Cons.length |> List.range 0

        notes =
            chord
                |> chordToNotes
                |> Cons.toList
                |> List.map (\note -> (indexedFilterMap (fret note) openNotes))
                |> stringProduct voices
                |> List.map fretsToGrip
    in
        notes


fretsToGrip : List { b | fret : a, string : Int } -> Six (Maybe a)
fretsToGrip xs =
    let
        f { string, fret } grip =
            sixMapOne string grip (Just fret)
    in
        List.foldl f emptySix xs


sixMapOne : Int -> Six a -> a -> Six a
sixMapOne n six x =
    case n of
        0 ->
            { six | one = x }

        1 ->
            { six | two = x }

        2 ->
            { six | three = x }

        3 ->
            { six | four = x }

        4 ->
            { six | five = x }

        5 ->
            { six | six = x }

        _ ->
            six


{-| Pick up one string for each note. Discard if a string is repeated.
-}
stringProduct :
    List Int
    -> List (List ( comparable, Int ))
    -> List (List { string : comparable, voice : Int, fret : Int })
stringProduct voices notesThenStrings =
    let
        f voice ( string, fret ) =
            ( string, { string = string, voice = voice, fret = fret } )

        g i xs =
            xs |> List.map (f i)

        h ( key, value ) dict =
            Dict.update key (h_ value) dict

        h_ x xs =
            case xs of
                Just xs_ ->
                    Just (x :: xs_)

                Nothing ->
                    Just [ x ]

        containsAllVoices xs =
            xs |> List.map .voice |> (contains voices)

        bigStretch xs =
            xs |> List.map .fret |> stretch |> (\x -> x > 3)
    in
        notesThenStrings
            |> List.indexedMap g
            |> List.concatMap identity
            |> List.foldl h Dict.empty
            |> Dict.toList
            |> List.map (\( a, b ) -> b)
            |> productWithEmpty
            |> List.filter containsAllVoices
            |> List.filter (not << bigStretch)


productWithEmpty : List (List b) -> List (List b)
productWithEmpty xs =
    xs
        |> List.map (\ys -> Nothing :: (List.map Just ys))
        |> product
        |> List.map (List.filterMap identity)


stretch : List Int -> Int
stretch xs =
    xs
        |> List.filter ((/=) 0)
        |> Cons.fromList
        |> Maybe.map (\xs -> (Cons.maximum xs - Cons.minimum xs))
        |> Maybe.withDefault 0


{-| true if xs contains all ys
-}
contains : List a -> List a -> Bool
contains ys xs =
    ys |> List.all (\y -> List.member y xs)



-- |> List.map f


indexedFilterMap : (a -> Maybe b) -> List a -> List ( number, b )
indexedFilterMap f =
    let
        inner n xs =
            case xs of
                x :: rest ->
                    case f x of
                        Just y ->
                            ( n, y ) :: inner (n + 1) rest

                        Nothing ->
                            inner (n + 1) rest

                [] ->
                    []
    in
        inner 0


filterMaybeSix : Six (Maybe a) -> List ( Int, a )
filterMaybeSix =
    let
        f ( i, a ) =
            case a of
                Just a_ ->
                    Just ( i, a_ )

                Nothing ->
                    Nothing
    in
        sixToList >> List.indexedMap (,) >> List.filterMap f


sixToList : Six a -> List a
sixToList { one, two, three, four, five, six } =
    [ one, two, three, four, five, six ]


emptySix : Six (Maybe a)
emptySix =
    { one = Nothing
    , two = Nothing
    , three = Nothing
    , four = Nothing
    , five = Nothing
    , six = Nothing
    }


mapSix : (a -> b) -> Six a -> Six b
mapSix f { one, two, three, four, five, six } =
    { one = f one
    , two = f two
    , three = f three
    , four = f four
    , five = f five
    , six = f six
    }


standard : Six Note
standard =
    let
        one =
            { letter = E, octave = 3 } |> fullNoteToNote
    in
        { one = one
        , two = one + 5
        , three = one + 5 + 5
        , four = one + 5 + 5 + 5
        , five = one + 5 + 5 + 5 + 4
        , six = one + 5 + 5 + 5 + 4 + 5
        }


type alias Six a =
    { one : a
    , two : a
    , three : a
    , four : a
    , five : a
    , six : a
    }


type alias Tuning =
    Six Int


type alias Grip =
    Six (Maybe Int)
