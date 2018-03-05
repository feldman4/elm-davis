module Audio.Widgets exposing (..)

import Audio.Draw
import Audio.Fretted exposing (chordToGrips, standardE)
import Audio.Fretboard exposing (drawGrips)
import Audio.Ladder
import Audio.Midi
import Audio.Music
import Audio.Roll
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Html exposing (div, program, text)
import Html.Attributes exposing (style)
import List.Extra
import Cons
import NativeModule


-- PERFORMANCE


memoChordToGripsStandardE :
    Audio.Music.Rooted Audio.Music.Chord
    -> List Audio.Fretted.Grip
memoChordToGripsStandardE =
    NativeModule.memoize (chordToGrips standardE)



-- WIDGETS


pianoRoll :
    { a | noteHistory : List NoteEvent, notes : List NoteEvent, time : Float }
    -> b
    -> Html.Html msg
pianoRoll model configs =
    (model.notes ++ model.noteHistory)
        |> List.take 100
        |> List.map (relativeToPresent model.time)
        |> Audio.Roll.buildRoll 3
        |> Audio.Roll.rollToSvg Audio.Roll.rollNoteToSvg
        |> Audio.Draw.svgScene
        |> (\x -> div divAttributes [ x ])


notesToText : { b | notes : List { a | note : Int } } -> c -> Html.Html msg
notesToText model configs =
    model.notes
        |> List.map .note
        |> List.map (Audio.Music.noteToFullNote >> printFullNote)
        |> String.join " "
        |> (\s ->
                if String.length s == 0 then
                    "no notes"
                else
                    s
           )
        |> text
        |> (\x -> div [] [ x ])


guitarVoicings : NoteModel a -> String -> String -> List String -> Html.Html msg
guitarVoicings model guitarFlag pianoFlag configs =
    (chord model guitarFlag pianoFlag configs)
        |> List.take 4
        |> cons2fromList
        |> Maybe.map Audio.Music.notesToChord
        |> Maybe.map (Audio.Music.allInversionsWindow 5)
        |> Maybe.map (List.concatMap memoChordToGripsStandardE)
        |> Maybe.withDefault []
        |> List.sortBy Audio.Fretted.difficulty
        |> List.take 8
        |> drawGrips


ladderHtml :
    NoteModel a
    -> String
    -> String
    -> String
    -> List String
    -> Html.Html msg
ladderHtml model guitarFlag pianoFlag triadFlag configs =
    let
        root =
            model.noteHistory
                |> Audio.Midi.establishRoot
                |> Maybe.withDefault (Audio.Music.fullNoteToNote Audio.Music.middleC % 12)

        chord_ =
            (chord model guitarFlag pianoFlag configs)

        update =
            if List.member triadFlag configs then
                Audio.Ladder.haloTriad (model.notes |> List.map .note |> List.map (\x -> (x - root) % 12))
            else
                halo chord_

        lightenUnplayedNotes chord saturation lightness =
            Audio.Ladder.updateStepsSimple
                (Audio.Ladder.selectNotes root chord >> not)
                ((Audio.Ladder.saturationStep saturation) >> (Audio.Ladder.lightnessStep lightness))

        halo chord =
            case chord |> Cons.fromList of
                Just xs ->
                    (Audio.Ladder.haloLeading root xs)

                Nothing ->
                    (\_ _ a -> a)
    in
        Audio.Music.bigFour
            |> Audio.Ladder.modesToLadder
            |> Audio.Ladder.mapStep Audio.Ladder.colorByQuality
            |> lightenUnplayedNotes chord_ 0.6 0.3
            |> Audio.Ladder.updateSteps (\_ _ _ -> True) update
            |> Audio.Ladder.ladderToSvg Audio.Ladder.stepToSvg
            |> Audio.Draw.svgScene
            |> (\x -> div divAttributes [ x ])



-- HELPERS


divAttributes : List (Html.Attribute msg)
divAttributes =
    [ style [ ( "width", "50%" ), ( "margin", "0 auto" ), ( "display", "block" ) ] ]


chordPiano : NoteModel a -> List Int
chordPiano model =
    (model.notes ++ model.noteHistory)
        |> Audio.Midi.lastChordPiano
        |> List.map .note
        |> List.Extra.unique


chordGuitar : NoteModel a -> List Int
chordGuitar model =
    (model.notes ++ model.noteHistory)
        |> Audio.Midi.lastChordPiano
        |> List.map .note
        |> List.Extra.unique


chord : NoteModel a -> String -> String -> List String -> List Int
chord model guitarFlag pianoFlag configs =
    if List.member guitarFlag configs then
        chordGuitar model
    else if List.member pianoFlag configs then
        chordPiano model
    else
        model.notes |> List.map .note |> List.Extra.unique


chordText : NoteModel a -> String -> String -> List String -> Html.Html msg
chordText model guitarFlag pianoFlag configs =
    let
        chordNames =
            chord model guitarFlag pianoFlag configs
                |> Audio.Draw.printPossibleChords
    in
        case chordNames of
            [] ->
                div [] [ text "no chord" ]

            _ ->
                chordNames
                    |> String.join ", "
                    |> (\x -> div [] [ text x ])
