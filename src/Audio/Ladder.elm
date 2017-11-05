module Audio.Ladder
    exposing
        ( modesToLadder
        , ladderToSvg
        , stepToSvg
        , updateSteps
        , updateStepsSimple
        , colorStep
        , alphaStep
        , saturationStep
        , lightnessStep
        , selectNotes
        , mapStep
        , colorByQuality
        , haloTriad
        , haloLeading
        )

import Audio.Draw exposing (..)
import Audio.Leading exposing (..)
import Audio.Music exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Color exposing (Color, black, blue, gray, red, rgba)
import Cons exposing (Cons, cons)
import Dict exposing (Dict)
import List.Extra
import Math.Matrix4 as M4 exposing (Mat4)
import TypedSvg exposing (circle, g, line, path, rect, svg)
import TypedSvg.Attributes as SA exposing (noFill, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (percent, px)


-- BUILD


modesToLadder : List Mode -> Ladder Mode StepData msg
modesToLadder scales =
    let
        step ( n, y ) =
            { data = { interval = n, arcs = [], color = rgba 1 1 1 1 }
            , attributes = [ stroke (rgba 0 0 0 1) ]
            , transform = M4.identity |> M4.translate3 0 (1 - y) 0
            }

        rail steps =
            { data = steps
            , attributes = []
            , transform = M4.identity
            }

        double xs =
            xs

        positions =
            modeToIntervals >> double >> intervalsToPositions

        rootIntervals =
            modeToIntervals >> double >> (List.scanl (+) 0)

        makeRail scaleFromCenter =
            scaleFromCenter
                |> (\x -> List.Extra.zip (rootIntervals x) (positions x))
                |> List.map step
                |> rail

        rails =
            scales |> List.map (\b -> ( b, makeRail b ))
    in
        { rails = rails, rungs = Dict.empty }



-- MODIFY


createArcs : Int -> List Arc
createArcs n_ =
    let
        n =
            toFloat n_
    in
        List.range 0 (n_ - 1)
            |> List.map
                (\i ->
                    { start = (1 / n) * (toFloat i + 0.09) + (1 / 12)
                    , stop = (1 / n) * ((toFloat i) + 0.9) + (1 / 12)
                    , color = rgba 150 150 150 1
                    , voice = 0
                    , index = i
                    }
                )


{-| Create arcs for 1-3-5 within the provided mode, if they are in the list of
intervals.
-}
haloTriad : List Int -> Mode -> Int -> Step StepData msg -> Step StepData msg
haloTriad intervals mode k step =
    let
        buildArc i interval =
            ( interval
            , { start = (1 / 3) * (toFloat i) + 0.03 + (1 / 12)
              , stop = (1 / 3) * ((toFloat i) + 1) - 0.03 + (1 / 12)
              , color = rgba 150 150 150 1
              , voice = 0
              }
            )

        played ( arc, interval ) =
            List.member (interval % 12) (List.map (\x -> x % 12) intervals)

        data =
            step.data
    in
        { mode | mode = (mode.mode + k) }
            |> modeToChord (cons 2 [ 4 ])
            |> .intervals
            |> Cons.toList
            |> (::) 0
            |> List.map ((+) step.data.interval)
            |> List.Extra.zip (createArcs 3)
            |> List.filter played
            |> List.unzip
            |> (\( arcs, _ ) -> { data | arcs = arcs })
            |> (\data -> { step | data = data })


{-| Create arcs indicating voice leading into the 1-3-5 at this step.

Position the arcs according to the currently played tone order (low to high).
Only lead with bottom 3 tones. When 1 tone is played, triple it to get low and
high leading. When 2 tones are played, double the higher or lower depending on
range. When nothing is played, show nothing.

Indicate the amount of voice leading using left- or right-aligned dots for
upward and downward voice leading. Also try showing hollow dots for downward leading.

Use a solid bar if the voice matches.

Hide for the detected root? What to show if a triad is not being played?

1. show voices in order lowest to highest
- need to pick 3, could remove octave doublings and go with lowest
  - screws up common voicings like Maj7 rooted on A (1-5-7-3)
  - just support 4, covers most guitar voicings. show nothing for the unused
  voice (can extend later for 7th chords)

2. show voices in order of role in current chord (i.e., remove inversion)

Steps:
1. for each of 1-3-5 at this step, is it in played notes? if so, remove both
and set the voice leading to zero
2. for remaining 1-3-5 x played notes, do linear assignment with cost proportional
to size of voice leading. brute force scales as (n choose k) where n = played notes
and k = remaining 1-3-5

(5 3) = 5! / (2! * 3!) = 10
not too bad

(9 3) = 9 *  8 * 7 / 6 = 3 * 4 * 7 = 84
worst case, could cache if needed

-}
haloLeading : Int -> Cons Note -> Mode -> Int -> Step StepData msg -> Step StepData msg
haloLeading root notes mode k step =
    let
        closestLeading =
            notes
                |> Cons.toList
                |> List.sort
                |> cons2fromList
                |> Maybe.map notesToChord
                |> Maybe.map (\c -> findLeading c modePlayed)
                |> Maybe.andThen List.head

        modePlayed =
            { scale = mode.scale, mode = mode.mode, root = root }
                |> relativeRootedMode k
    in
        case closestLeading of
            Just ([ ( a1, b1 ), ( a2, b2 ), ( a3, b3 ) ] as xs) ->
                createArcs 3
                    |> List.Extra.zip xs
                    |> List.map (\( ( a, b ), arc ) -> { arc | voice = (b - a) })
                    |> (\arcs ->
                            mapData (\d -> { d | arcs = arcs }) step
                       )

            _ ->
                step


colorByQuality : Mode -> Int -> Step StepData msg -> Step StepData msg
colorByQuality mode k step =
    let
        quality =
            { mode | mode = mode.mode + k } |> chordInScale

        color =
            case quality of
                Nothing ->
                    step.data.color

                Just Major ->
                    rgba 250 100 100 1

                Just Minor ->
                    rgba 100 100 250 1

                Just Diminished ->
                    rgba 235 175 0 1

                Just Augmented ->
                    rgba 100 250 100 1
    in
        mapData (\x -> { x | color = color }) step


selectNotes : Note -> List Note -> Step StepData msg -> Bool
selectNotes root noteList step =
    let
        intervals =
            noteList
                |> List.map (\x -> (x - root) % 12)
    in
        List.member (step.data.interval % 12) intervals


mapStepColor : (Color -> Color) -> Step StepData msg -> Step StepData msg
mapStepColor f step =
    f step.data.color
        |> (\c -> mapData (\x -> { x | color = c }) step)


colorStep : Color -> Step StepData msg -> Step StepData msg
colorStep color =
    mapStepColor (\_ -> color)


alphaStep : Float -> Step StepData msg -> Step StepData msg
alphaStep alpha step =
    step.data.color
        |> Color.toRgb
        |> (\c -> Color.rgba c.red c.green c.blue alpha)
        |> (\c -> mapData (\x -> { x | color = c }) step)


saturationStep : Float -> Step StepData msg -> Step StepData msg
saturationStep saturation step =
    step.data.color
        |> Color.toHsl
        |> (\c -> Color.hsla c.hue saturation c.lightness c.alpha)
        |> (\c -> mapData (\x -> { x | color = c }) step)


lightnessStep : Float -> Step StepData msg -> Step StepData msg
lightnessStep lightness step =
    let
        compress a b =
            1 - (1 - a) * b
    in
        step.data.color
            |> Color.toHsl
            |> (\c -> Color.hsla c.hue c.saturation (compress c.lightness lightness) c.alpha)
            |> (\c -> mapData (\x -> { x | color = c }) step)



-- DRAW


dotAngles : Int -> Arc -> List Float
dotAngles maxVoice arc =
    let
        voices =
            arc.voice
                |> abs
                |> clamp 0 maxVoice
                |> List.range 1
                |> List.map position

        position x =
            (arc.stop - arc.start) / (toFloat maxVoice) * (-0.5 + toFloat x)
    in
        if arc.voice > 0 then
            voices |> List.map (\x -> arc.start + x)
        else
            voices |> List.map (\x -> arc.stop - x)


{-| dots along arcs, radius in user units
-}
drawDots : Float -> Int -> Arc -> List (Svg msg)
drawDots radius maxVoice arc =
    let
        dotRadius =
            (arc.stop - arc.start)
                / (toFloat maxVoice)
                |> (*) (0.3 * radius * 2 * pi)

        color =
            if arc.voice > 0 then
                (rgba 50 50 50 1)
            else
                (rgba 50 150 50 1)

        dot angle =
            circle
                [ SA.r (percent (100 * dotRadius))
                , strokeWidth (percent (10 * dotRadius))
                , stroke color
                , SA.cx (percent (100 * radius * cos angle))
                , SA.cy (percent (100 * radius * sin angle))
                , fill color
                ]
                []
    in
        dotAngles maxVoice arc
            |> List.map ((*) (2 * pi))
            |> List.map dot


drawDotsSide : Int -> Int -> Int -> List (Svg msg)
drawDotsSide maxVoice index voice =
    let
        color =
            if voice > 0 then
                (rgba 50 50 50 1)
            else
                (rgba 50 150 50 1)

        dotRadius =
            (0.5 * 1.6) / (toFloat maxVoice)

        xOffset =
            xInc * 3 + 1.6

        xInc =
            dotRadius * 2.2

        yInc =
            dotRadius * 2.7

        place j =
            circle
                [ SA.cy (percent (100 * yInc * (1 - toFloat index)))
                , SA.cx (percent (100 * (xOffset + toFloat j * xInc * (sign voice))))
                , stroke color
                , strokeWidth (percent 0)
                , fill color
                , SA.r (percent (100 * dotRadius))
                ]
                []
    in
        List.range 0 (abs voice - 1)
            |> List.map place


arcToSvg : Step StepData msg -> Int -> Arc -> Svg msg
arcToSvg step index arc =
    let
        radius =
            1.6

        width =
            percent 20

        -- dotSvg =
        --     drawDots radius 3 arc
        dotSvg =
            drawDotsSide 3 index arc.voice

        arcSvg =
            if arc.voice == 0 then
                [ path
                    ([ SA.d (arcCommand (arc.start * 2 * pi) (arc.stop * 2 * pi) radius)
                     , noFill
                     , stroke arc.color
                     , strokeWidth width
                     , opacity (step.data.color |> Color.toRgb |> .alpha |> (\x -> x ^ 2))
                     ]
                    )
                    []
                ]
            else
                []
    in
        g [] (arcSvg ++ dotSvg)


stepToSvg : Step StepData msg -> Svg msg
stepToSvg step =
    let
        position =
            [ step.transform |> M4.scale3 0.025 0.025 1 |> transformToAttribute ]

        arcs =
            step.data.arcs |> List.indexedMap (arcToSvg step)
    in
        g position
            ([ circle
                ([ SA.r (percent 100)
                 , stroke black
                 , strokeWidth (percent 28)
                 , fill step.data.color
                 , opacity (step.data.color |> Color.toRgb |> .alpha)
                 ]
                    ++ step.attributes
                )
                []
             ]
                ++ arcs
            )


railToSvg : (Step a msg -> Svg msg) -> Rail a msg -> Svg msg
railToSvg drawStep rail =
    let
        barPosition =
            [ SA.x1 (percent 0)
            , SA.x2 (percent 0)
            , SA.y1 (percent 0)
            , SA.y2 (percent 100)
            , transformToAttribute rail.transform
            ]

        barStroke =
            [ stroke (gray), strokeWidth (percent 1) ]

        bar =
            line (barPosition ++ barStroke) []

        noteCircles =
            rail
                |> .data
                |> List.map drawStep
                |> (\xs -> g [ transformToAttribute rail.transform ] xs)
    in
        g [] [ bar, noteCircles ]


ladderToSvg : (Step b msg -> Svg msg) -> Ladder a b msg -> Svg msg
ladderToSvg drawStep ladder =
    let
        n =
            ladder.rails |> List.length |> toFloat

        adjust i ( name, rail ) =
            rail
                |> scaleFromCenter 0.9 0.9
                |> translate ((i |> toFloat |> (+) 0.5) / n) 0
                |> railToSvg drawStep
    in
        ladder.rails
            |> List.indexedMap adjust
            |> (\xs -> g [] xs)



--HELPERS


{-| provide select and update functions to create a ladder transformer.
-}
updateSteps : (a -> Int -> Step b msg -> Bool) -> (a -> Int -> Step b msg -> Step b msg) -> Ladder a b msg -> Ladder a b msg
updateSteps select update =
    select ?? update |> mapStep


{-| provide select and update functions. simple functions don't need rail info.
-}
updateStepsSimple : (Step b msg -> Bool) -> (Step b msg -> Step b msg) -> Ladder a b msg -> Ladder a b msg
updateStepsSimple select update =
    (\a b c -> select c) ?? (\a b c -> update c) |> mapStep


(??) : (a -> b -> c -> Bool) -> (a -> b -> c -> c) -> a -> b -> c -> c
(??) f g a b c =
    if f a b c then
        g a b c
    else
        c


addAttribute : a -> { b | attributes : List a } -> { b | attributes : List a }
addAttribute attr x =
    { x | attributes = x.attributes ++ [ attr ] }


tScale : Float -> Float -> TypedSvg.Types.Transform
tScale =
    TypedSvg.Types.Scale


tTranslate : Float -> Float -> TypedSvg.Types.Transform
tTranslate =
    TypedSvg.Types.Translate


mapStep : (a -> Int -> Step b msg -> Step c msg) -> Ladder a b msg -> Ladder a c msg
mapStep f ladder =
    let
        mapRail name =
            mapData (List.indexedMap (f name))
    in
        ladder.rails
            |> List.map (\( name, rail ) -> ( name, mapRail name rail ))
            |> (\x -> { ladder | rails = x })



-- TYPES


type alias Step data msg =
    WrappedSvg data msg


type alias Rail data msg =
    WrappedSvg (List (Step data msg)) msg


type alias Rung msg =
    WrappedSvg {} msg


type alias Arc =
    { start : Float, stop : Float, color : Color, voice : Int, index : Int }


type alias StepData =
    { interval : Int, arcs : List Arc, color : Color }


{-| awkward to store name in tuple list, awkward to modify Rail to store name
-}
type alias Ladder railName data msg =
    { rails : List ( railName, Rail data msg )
    , rungs : Dict ( ( railName, Int ), ( railName, Int ) ) (Rung msg)
    }
