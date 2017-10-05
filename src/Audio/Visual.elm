module Audio.Visual exposing (..)

import Audio.GL exposing (..)
import Audio.Music exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Color exposing (Color, black, gray, red, rgba)
import Html exposing (Html, div, text)
import List.Extra
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes as SA exposing (fill, noFill, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (percent, px)


{-

   svg conventions
   - viewbox _ _ x y establishes units used by transform(..), pick x = y = 1

   draw piano roll
   - one octave, inner lines for note octave
   - List NoteEvent
     - time is treated as a percent, apply higher level transforms to set scale/bounds
   - group svg with <g> to apply common attributes
    - can also select svg class tags with css

-}


tScale : Float -> Float -> TypedSvg.Types.Transform
tScale =
    TypedSvg.Types.Scale


tTranslate : Float -> Float -> TypedSvg.Types.Transform
tTranslate =
    TypedSvg.Types.Translate


eventPosition : Float -> Maybe Float -> List (TypedSvg.Core.Attribute msg)
eventPosition start end =
    [ SA.x1 (percent (end |> Maybe.withDefault 0)), SA.x2 (percent start) ]


svgScale : Float -> List NoteEvent -> List (Svg msg)
svgScale duration noteEvents =
    let
        geometry =
            [ SA.x (percent 0), SA.y (percent 0), SA.width (percent 100), SA.height (percent 100) ]

        paint =
            [ stroke gray, noFill, SA.strokeWidth (percent 0) ]

        box =
            rect (geometry ++ paint) []

        toBar y =
            line
                [ SA.x1 (percent 0)
                , SA.x2 (percent 100)
                , SA.y1 (percent (100 * y))
                , SA.y2 (percent (100 * y))
                , stroke black
                , strokeWidth (percent 1.5)
                ]
                []

        bars =
            List.range 0 11
                |> List.map toFloat
                |> List.map (\x -> (x / 11))
                |> List.map toBar
                |> g [ transform [ tTranslate 0 0.025, tScale 1 0.95 ] ]

        adjustDuration x =
            100 * (min 1 (x / duration))

        draw noteEvent =
            let
                xPosition =
                    eventPosition (adjustDuration noteEvent.start) (noteEvent.end |> (Maybe.map adjustDuration))

                yPosition =
                    noteEvent.note.letter
                        |> letterToPosition
                        |> (\y -> 1 - ((toFloat y) + 0.5) / 11)
                        |> (\y -> [ SA.y1 (percent (100 * y)), SA.y2 (percent (100 * y)) ])
            in
                line (xPosition ++ yPosition ++ [ stroke red, strokeWidth (percent 1) ]) []

        notes =
            noteEvents
                |> List.filter (\n -> n.end |> Maybe.map ((>) duration) |> Maybe.withDefault True)
                |> List.map draw
                |> g [ transform [ tTranslate 0.05 0.025, tScale 0.9 0.95 ] ]
    in
        [ box, bars, notes ]


printPossibleChords : List Note -> Html msg
printPossibleChords noteList =
    noteList
        |> List.sortBy noteToInt
        |> cons2fromList
        |> Maybe.map notesToChord
        |> Maybe.map allInversions
        |> Maybe.map ((List.map printChord) >> Maybe.Extra.values)
        |> Maybe.withDefault []
        |> List.Extra.unique
        |> String.join ", "
        |> (\x -> div [] [ text x ])


printChord : Rooted Chord -> Maybe String
printChord chord =
    let
        root =
            chord.root.letter |> letterToString

        quality =
            chord |> getChordQuality

        print =
            (\a b -> [ a, toString b ] |> String.join " ")
    in
        Maybe.map2 print (Just root) quality


svgScene : List (Svg a) -> Html a
svgScene =
    svg [ SA.width (percent 100), SA.height (percent 100), viewBox 0 0 1 1 ]


noteToAttr : Note -> Attributes
noteToAttr { letter, octave } =
    let
        color =
            letter |> letterToColor

        offset =
            (letter |> letterToPosition) |> toFloat |> (*) 0.1

        position =
            vec3 (((octave - 2 |> toFloat) + offset) * 0.25) (((octave - 2 |> toFloat) + offset) * 0.25) 0
                |> (flip Math.Vector3.add) (vec3 -0.5 -0.5 0)
    in
        { position = position, color = color }


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
