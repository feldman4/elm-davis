module Audio.Roll exposing (buildRoll, rollToSvg, rollNoteToSvg)

import Audio.Draw exposing (..)
import Audio.Types exposing (..)
import Color exposing (Color, black, gray, red, rgba)
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes as SA exposing (fill, noFill, stroke, strokeWidth, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (percent, px)
import Math.Matrix4 as M4


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
-- BUILD


buildRoll : Float -> List NoteEvent -> Roll msg
buildRoll duration noteEvents =
    let
        notes =
            noteEvents
                |> List.filter (\n -> n.end |> Maybe.map ((>) duration) |> Maybe.withDefault True)
                |> List.map (\n -> { color = rgba 200 50 50 1, noteEvent = n })
                |> List.map (\d -> { data = d, attributes = [], transform = M4.identity })
    in
        { notes = notes, duration = duration }



-- DRAW


rollNoteToSvg : RollNote msg -> Svg msg
rollNoteToSvg rollNote =
    let
        octave =
            rollNote.data.noteEvent.note |> toFloat |> (\x -> -1 + floor (x / 12))

        ( yOffset, penThickness ) =
            if octave <= 2 then
                ( 2, 0.016 )
            else if octave == 3 then
                ( 0.7, 0.014 )
            else if octave == 4 then
                ( -0.4, 0.012 )
            else if octave == 5 then
                ( -1.3, 0.01 )
            else
                ( -2, 0.008 )

        yPosition =
            rollNote.data.noteEvent.note
                |> (\x -> 1 - (toFloat (x % 12)) / 12)
                |> (\x -> x - (1 / 24))
                |> (+) (yOffset * 0.015)

        end =
            rollNote.data.noteEvent.end
                |> Maybe.withDefault 0

        transform =
            rollNote
                |> translate end yPosition
                |> scale (rollNote.data.noteEvent.start - end) 1
                |> .transform

        attributes =
            [ transformToAttribute transform
            , stroke rollNote.data.color
            , strokeWidth (percent (100 * penThickness))
            ]
    in
        g attributes [ horizontalLine ]


rollToSvg : (RollNote msg -> Svg msg) -> Roll msg -> Svg msg
rollToSvg drawNote roll =
    let
        barWidth =
            0.005

        transformStaff i =
            M4.identity
                |> M4.translate3 0 (toFloat i / 12) 0

        transformNote =
            M4.identity
                |> M4.scale3 (1 / roll.duration) 1 1
                |> M4.translate3 0 0 0
                |> scaleFromCenter3 0.9 1 1

        transformRoll =
            M4.identity
                |> M4.translate3 0 0.05 0
                |> M4.scale3 1 0.9 1

        limitDuration noteEvent =
            { noteEvent | start = min roll.duration noteEvent.start }

        notes =
            roll.notes
                |> List.map (mapData (\x -> { x | noteEvent = limitDuration x.noteEvent }))
                |> List.map drawNote
                |> g [ transformToAttribute transformNote ]

        staffBar i =
            g
                [ stroke black
                , strokeWidth (percent (100 * barWidth))
                , transformToAttribute (transformStaff i)
                ]
                [ horizontalLine ]

        staff =
            List.range 0 12 |> List.map staffBar |> g []
    in
        g [ transformToAttribute transformRoll ] [ staff, notes ]


type alias RollNote msg =
    WrappedSvg RollNoteData msg


type alias RollNoteData =
    { color : Color, noteEvent : NoteEvent }


type alias Roll msg =
    { notes : List (RollNote msg), duration : Float }
