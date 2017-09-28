module Audio.Visual exposing (..)

import Audio.Types exposing (..)
import Audio.Music exposing (..)
import Audio.GL exposing (..)
import Math.Vector3 exposing (vec3, Vec3)
import TypedSvg exposing (svg, circle, rect, line)
import TypedSvg.Attributes as SA exposing (viewBox, width, height, rx, ry, cx, cy, r, noFill, fill, strokeWidth, stroke)
import TypedSvg.Types exposing (px, percent)
import TypedSvg.Core exposing (Svg)
import Html
import Color exposing (Color, rgba, gray, black)


{-

   draw piano roll
   - one octave, inner lines for note octave
   - List NoteEvent
     - time is treated as a percent, apply higher level transforms to set scale/bounds
   - group svg with <g> to apply common attributes
    - can also select svg class tags with css

-}


type alias NoteEvent =
    { note : Note, start : Float, end : Maybe Float }


svgEvent : Float -> Maybe Float -> List a
svgEvent start end =
    let
        position =
            [ SA.x1 (percent (end |> Maybe.withDefault 0)), SA.x2 (percent start) ]
    in
        []


svgScale : List Note -> List (Svg msg)
svgScale notes =
    let
        geometry =
            [ SA.x (percent 0), SA.y (percent 0), width (percent 100), height (percent 100), rx (px 15), ry (px 15) ]

        paint =
            [ stroke gray, noFill, SA.strokeWidth (percent 2) ]

        toLine y =
            line
                [ SA.x1 (percent 10)
                , SA.x2 (percent 90)
                , SA.y1 (percent (100 * y))
                , SA.y2 (percent (100 * y))
                , stroke black
                , strokeWidth (percent 1.5)
                ]
                []

        bars =
            List.range 0 11
                |> List.map toFloat
                |> List.map (\x -> (x + 0.5) * (1 / 12))
                |> List.map toLine
    in
        [ rect (geometry ++ paint) [] ] ++ bars


svgScene : List (Svg a) -> Html.Html a
svgScene =
    svg [ width (px 300), height (px 300), viewBox 0 0 120 120 ]


roundRect : List (Svg msg)
roundRect =
    [ rect [ SA.x (px 10), SA.y (px 10), width (percent 80), height (px 100), rx (px 15), ry (px 15) ] [] ]


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
