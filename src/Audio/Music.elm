module Audio.Music exposing (..)

import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Cons exposing (Cons, cons)


{-| represents a scale and a mode within the scale
-}
type alias Mode =
    { scale : Scale, mode : Int }


type Scale
    = NaturalMinor
    | NaturalMajor
    | MelodicMinor
    | MelodicMajor
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
    , root = { letter = A, octave = 3 } |> fullNoteToNote
    }


exampleRootedMajor : Rooted Chord
exampleRootedMajor =
    { intervals = exampleMajor.intervals
    , root = { letter = A, octave = 3 } |> fullNoteToNote
    }


exampleInvertedMajor : Rooted Chord
exampleInvertedMajor =
    exampleRootedMajor |> invertRootedChord


exampleRootedMode : Rooted Mode
exampleRootedMode =
    { scale = NaturalMajor
    , mode = 2
    , root = { letter = E, octave = 4 } |> fullNoteToNote
    }



--


aMinor : Rooted Chord
aMinor =
    { intervals = exampleMinor.intervals
    , root = { letter = A, octave = 3 } |> fullNoteToNote
    }


cMajor : Rooted Chord
cMajor =
    { intervals = exampleMajor.intervals
    , root = { letter = C, octave = 4 } |> fullNoteToNote
    }


dMajor7 : Rooted Chord
dMajor7 =
    Mode NaturalMajor 0
        |> modeToChord (Cons.cons 2 [ 4, 6 ])
        |> rootChord ({ letter = D, octave = 4 } |> fullNoteToNote)


majorScale : Mode
majorScale =
    Mode NaturalMajor 0



--


rootChord : Note -> Chord -> Rooted Chord
rootChord note chord =
    { root = note, intervals = chord.intervals }


bigFour : List Mode
bigFour =
    -- [ MelodicMajor, HarmonicMinor, MelodicMinor, HarmonicMajor ]
    --     |> List.map (\x -> Mode x 0)
    [ Mode MelodicMajor 0, Mode MelodicMajor 5 ]


analyzeChord : Chord_ a -> { quality : Maybe ChordQuality }
analyzeChord chord =
    { quality = getChordQuality chord }


chordToNotes : Rooted Chord -> Cons Int
chordToNotes { intervals, root } =
    intervals
        |> Cons.map (\n -> n + root)
        |> Cons.append (Cons.cons (root) [])


modeToBaseIntervals : Mode -> List Int
modeToBaseIntervals =
    modeToIntervals >> List.scanl (+) 0


{-| mode root is not currently limited to [0, 11]
-}
relativeRootedMode : Int -> Rooted Mode -> Rooted Mode
relativeRootedMode step { scale, mode, root } =
    { scale = scale, mode = mode }
        |> modeToIntervals
        |> List.scanl (+) 0
        |> rotate step
        |> List.head
        |> Maybe.withDefault 0
        |> (\x -> { scale = scale, mode = mode + step, root = root + x })


modeToIntervals : Mode -> List Int
modeToIntervals { scale, mode } =
    case ( scale, mode ) of
        ( NaturalMajor, mode ) ->
            rotate mode [ 2, 2, 1, 2, 2, 2, 1 ]

        ( MelodicMajor, mode ) ->
            modeToIntervals { scale = NaturalMajor, mode = mode }

        ( NaturalMinor, mode ) ->
            modeToIntervals { scale = NaturalMajor, mode = (mode + 5) }

        ( HarmonicMinor, mode ) ->
            rotate mode [ 2, 1, 2, 2, 1, 3, 1 ]

        ( MelodicMinor, mode ) ->
            rotate mode [ 2, 1, 2, 2, 2, 2, 1 ]

        ( HarmonicMajor, mode ) ->
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


allInversionsWindow : Int -> Rooted Chord -> List (Rooted Chord)
allInversionsWindow window chord =
    let
        bottom =
            chord.root - window

        top =
            chord.intervals |> Cons.maximum |> (\x -> x + chord.root + window)
    in
        allInversions2 bottom top chord


allInversions2 : Note -> Note -> Rooted Chord -> List (Rooted Chord)
allInversions2 bottom top chord =
    let
        availableNotes note =
            let
                first =
                    ((note - bottom) % 12) + bottom
            in
                List.range 0 ((top - first) // 12)
                    |> List.map (\x -> x * 12 + first)
    in
        chord
            |> chordToNotes
            |> Cons.toList
            |> List.map availableNotes
            |> product
            |> List.map List.sort
            |> List.filterMap cons2fromList
            |> List.map notesToChord


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
            chord.root |> noteToFullNote |> .letter |> printLetter

        quality =
            chord |> getChordQuality

        print =
            (\a b -> [ a, toString b ] |> String.join " ")
    in
        Maybe.map2 print (Just root) quality



--


noteToFullNote : Int -> FullNote
noteToFullNote intNote =
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


fullNoteToNote : FullNote -> Note
fullNoteToNote note =
    note.letter |> letterToPosition |> (+) ((note.octave + 1) * 12)
