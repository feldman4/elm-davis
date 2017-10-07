module Audio.Music exposing (..)

import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Cons exposing (Cons, cons)


{-| represents a scale and a mode within the scale
-}
type Mode
    = Mode Scale Int


type Scale
    = NaturalMinor
    | NaturalMajor
    | HarmonicMajor
    | HarmonicMinor


type Triad
    = MinorTriad
    | MajorTriad


type ChordQuality
    = Diminished
    | Minor
    | Major
    | Augmented


type alias Chord =
    Chord_ {}


type alias Chord_ a =
    { a | intervals : Cons Int }


type alias Rooted a =
    { a | root : Note }


type alias Tones =
    { flatSecond : Bool
    , second : Bool
    , minorThird : Bool
    , third : Bool
    , fourth : Bool
    , flatFifth : Bool
    , fifth : Bool
    , flatSixth : Bool
    , sixth : Bool
    , seventh : Bool
    , majorSeventh : Bool
    }


middleC : FullNote
middleC =
    { letter = C, octave = 4 }


exampleMinor : Chord
exampleMinor =
    { intervals = cons 3 [ 7 ] }


exampleMajor : Chord
exampleMajor =
    { intervals = cons 4 [ 7 ] }


exampleRootedMinor : Rooted Chord
exampleRootedMinor =
    { intervals = exampleMinor.intervals
    , root = { letter = A, octave = 3 } |> noteToInt
    }


exampleRootedMajor : Rooted Chord
exampleRootedMajor =
    { intervals = exampleMajor.intervals
    , root = { letter = A, octave = 3 } |> noteToInt
    }


exampleInvertedMajor : Rooted Chord
exampleInvertedMajor =
    exampleRootedMajor |> invertRootedChord



--


aMinor : Rooted Chord
aMinor =
    { intervals = exampleMinor.intervals
    , root = { letter = A, octave = 3 } |> noteToInt
    }



-- |> invertRootedChord
-- |> invertRootedChord


cMajor : Rooted Chord
cMajor =
    { intervals = exampleMajor.intervals
    , root = { letter = C, octave = 4 } |> noteToInt
    }


fMajor2nd : { intervals : Cons number, root : { letter : Letter, octave : number1 } }
fMajor2nd =
    { intervals = cons 5 [ 9 ], root = { letter = C, octave = 4 } }


majorScale : Mode
majorScale =
    Mode NaturalMajor 0



--


rootChord : Note -> Chord -> Rooted Chord
rootChord note chord =
    { root = note, intervals = chord.intervals }


bigFour : List Mode
bigFour =
    -- [ NaturalMajor, NaturalMinor, HarmonicMinor, HarmonicMajor ]
    -- |> List.map (\x -> Mode x 0)
    [ Mode NaturalMinor 0, Mode NaturalMinor 2 ]


analyzeChord : Chord_ a -> { quality : Maybe ChordQuality }
analyzeChord chord =
    { quality = getChordQuality chord }


chordToNotes : Rooted Chord -> Cons Int
chordToNotes { intervals, root } =
    intervals
        |> Cons.map (\n -> n + root)
        |> Cons.append (Cons.cons (root) [])


modeToIntervals : Mode -> List Int
modeToIntervals scale =
    case scale of
        Mode NaturalMinor mode ->
            rotate mode [ 2, 1, 2, 2, 1, 2, 2 ]

        Mode NaturalMajor mode ->
            rotate mode [ 2, 2, 1, 2, 2, 2, 1 ]

        Mode HarmonicMinor mode ->
            rotate mode [ 2, 1, 2, 2, 1, 3, 1 ]

        Mode HarmonicMajor mode ->
            rotate mode [ 2, 2, 1, 2, 1, 3, 1 ]


classifyTriad : Int -> Maybe Triad
classifyTriad interval =
    case interval of
        3 ->
            Just MinorTriad

        4 ->
            Just MajorTriad

        _ ->
            Nothing


chordInScale : Mode -> Maybe ChordQuality
chordInScale scale =
    scale
        |> modeToChord (cons 2 [ 4 ])
        |> getChordQuality


notesToChord : Cons2 Note -> Rooted Chord
notesToChord notes =
    notes
        |> (\(Cons2 root first rest) ->
                { root = root
                , intervals = cons (first - root) (rest |> List.map (\x -> x - root))
                }
           )


modeToChord : Cons Int -> Mode -> Chord
modeToChord positions scale =
    let
        f n =
            scale |> modeToIntervals |> List.take n |> List.sum
    in
        { intervals = Cons.map f positions }


getChordTones : Chord_ a -> Tones
getChordTones { intervals } =
    let
        f n =
            intervals |> Cons.toList |> List.member n
    in
        { flatSecond = f 1
        , second = f 2
        , minorThird = f 3
        , third = f 4
        , fourth = f 5
        , flatFifth = f 6
        , fifth = f 7
        , flatSixth = f 8
        , sixth = f 9
        , seventh = f 10
        , majorSeventh = f 11
        }


allInversions : Rooted Chord -> List (Rooted Chord)
allInversions chordRooted =
    chordRooted.intervals
        |> Cons.toList
        |> List.scanl (\a b -> invertRootedChord b) chordRooted


invertChord : Chord_ a -> Chord
invertChord { intervals } =
    let
        ( first, rest ) =
            Cons.uncons intervals
    in
        cons 12 rest
            |> Cons.map (\x -> x - first)
            |> consRotate 1
            |> (\x -> { intervals = x })


invertRootedChord : Rooted Chord -> Rooted Chord
invertRootedChord chordRooted =
    let
        intervals =
            chordRooted |> invertChord |> .intervals

        root =
            chordRooted.root
                |> (+) (Cons.head chordRooted.intervals)
    in
        { intervals = intervals, root = root }


getChordQuality : Chord_ a -> Maybe ChordQuality
getChordQuality chord =
    let
        tones =
            chord |> getChordTones

        triadTones =
            [ .minorThird, .third, .flatFifth, .fifth, .flatSixth ]
                |> List.map (\f -> f tones)
    in
        case triadTones of
            [ True, False, True, False, False ] ->
                Just Diminished

            [ True, False, False, True, False ] ->
                Just Minor

            [ False, True, False, True, False ] ->
                Just Major

            [ False, True, False, False, True ] ->
                Just Augmented

            _ ->
                Nothing


getTriadQuality : Triad -> Triad -> ChordQuality
getTriadQuality a b =
    case ( a, b ) of
        ( MinorTriad, MinorTriad ) ->
            Diminished

        ( MinorTriad, MajorTriad ) ->
            Minor

        ( MajorTriad, MinorTriad ) ->
            Major

        ( MajorTriad, MajorTriad ) ->
            Augmented


printChord : Rooted Chord -> Maybe String
printChord chord =
    let
        root =
            chord.root |> intToNote |> .letter |> letterToString

        quality =
            chord |> getChordQuality

        print =
            (\a b -> [ a, toString b ] |> String.join " ")
    in
        Maybe.map2 print (Just root) quality
