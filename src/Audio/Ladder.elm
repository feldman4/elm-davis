module Audio.Ladder
    exposing
        ( modesToLadder
        , ladderToSvg
        , stepToSvg
        , updateSteps
        , updateStepsSimple
        , rgbStep
        , alphaStep
        , saturationStep
        , lightnessStep
        , selectNotes
        , mapStep
        , colorByQuality
        , haloTriad
        )

import Audio.Music exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Color exposing (Color, black, blue, gray, red, rgba)
import Cons exposing (cons)
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
            { data = { interval = n, arcs = [] }
            , attributes = [ stroke (rgba 0 0 0 1) ]
            , color = rgba 1 1 1 1
            , transform = M4.identity |> M4.translate3 0 (1 - y) 0
            }

        rail steps =
            { data = steps
            , attributes = []
            , transform = M4.identity
            , color = rgba 0 0 0 1
            }

        double xs =
            xs

        positions =
            scaleToIntervals >> double >> intervalsToPositions

        rootIntervals =
            scaleToIntervals >> double >> (List.scanl (+) 0)

        makeRail scale =
            scale
                |> (\x -> List.Extra.zip (rootIntervals x) (positions x))
                |> List.map step
                |> rail

        rails =
            scales |> List.map (\b -> ( b, makeRail b ))
    in
        { rails = rails, rungs = Dict.empty }



-- MODIFY


haloTriad : List Int -> Mode -> Int -> Step StepData msg -> Step StepData msg
haloTriad intervals (Mode mode n) k step =
    let
        buildArc i interval =
            ( interval
            , { start = (1 / 3) * (toFloat i) + 0.03 + (1 / 12)
              , stop = (1 / 3) * ((toFloat i) + 1) - 0.03 + (1 / 12)
              , color = rgba 150 150 150 1
              }
            )

        played ( interval, arc ) =
            List.member (interval % 12) (List.map (\x -> x % 12) intervals)

        data =
            step.data
    in
        Mode mode (n + k)
            |> formChord (cons 2 [ 4 ])
            |> .intervals
            |> Cons.toList
            |> (::) 0
            |> List.map ((+) step.data.interval)
            |> List.indexedMap buildArc
            |> List.filter played
            |> List.unzip
            |> (\( _, arcs ) -> { data | arcs = arcs })
            |> (\data -> { step | data = data })


colorByQuality : Mode -> Int -> Step a msg -> Step a msg
colorByQuality (Mode scale n) k step =
    let
        quality =
            (Mode scale (n + k)) |> chordInScale

        color =
            case quality of
                Nothing ->
                    step.color

                Just Major ->
                    rgba 250 100 100 1

                Just Minor ->
                    rgba 100 100 250 1

                Just Diminished ->
                    rgba 235 175 0 1

                Just Augmented ->
                    rgba 100 250 100 1
    in
        { step | color = color }


selectNotes : Letter -> List Note -> Step StepData msg -> Bool
selectNotes root noteList step =
    let
        rootInt =
            noteToInt { letter = root, octave = 0 }

        intervals =
            noteList
                |> List.map noteToInt
                |> List.map (\x -> (x - rootInt) % 12)
    in
        List.member (step.data.interval % 12) intervals


rgbStep : Color -> Step a msg -> Step a msg
rgbStep color step =
    color
        |> Color.toRgb
        |> (\c -> Color.rgba c.red c.green c.blue (step.color |> Color.toRgb |> .alpha))
        |> (\c -> { step | color = c })


alphaStep : Float -> Step a msg -> Step a msg
alphaStep alpha step =
    step.color
        |> Color.toRgb
        |> (\c -> Color.rgba c.red c.green c.blue alpha)
        |> (\c -> { step | color = c })


saturationStep : Float -> Step a msg -> Step a msg
saturationStep saturation step =
    step.color
        |> Color.toHsl
        |> (\c -> Color.hsla c.hue saturation c.lightness c.alpha)
        |> (\c -> { step | color = c })


lightnessStep : Float -> Step a msg -> Step a msg
lightnessStep lightness step =
    step.color
        |> Color.toHsl
        |> (\c -> Color.hsla c.hue c.saturation lightness c.alpha)
        |> (\c -> { step | color = c })



-- DRAW


drawArc : Step a msg -> List (TypedSvg.Core.Attribute msg) -> Arc -> Svg msg
drawArc step position { start, stop, color } =
    path
        ([ SA.d (arc (start * 2 * pi) (stop * 2 * pi) 0.04)
         , noFill
         , stroke color
         , strokeWidth (percent 0.5)
         , opacity (step.color |> Color.toRgb |> .alpha |> (\x -> x ^ 2))
         ]
            ++ position
        )
        []


stepToSvg : Step StepData msg -> Svg msg
stepToSvg step =
    let
        position =
            [ transform [ step.transform |> tMatrix ] ]

        arcs =
            step.data.arcs |> List.map (drawArc step position)
    in
        g []
            ([ circle
                ([ SA.x (percent 0)
                 , SA.y (percent 0)
                 , SA.r (percent 2.5)
                 , stroke black
                 , strokeWidth (percent 0.7)
                 , fill step.color
                 , opacity (step.color |> Color.toRgb |> .alpha)
                 ]
                    ++ position
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
            , transform [ tMatrix rail.transform ]
            ]

        barStroke =
            [ stroke black, strokeWidth (percent 1) ]

        bar =
            line (barPosition ++ barStroke) []

        noteCircles =
            rail
                |> transformChildren
                |> .data
                |> List.map drawStep
                |> (\xs -> g [] xs)
    in
        g [] [ bar, noteCircles ]


ladderToSvg : (Step b msg -> Svg msg) -> Ladder a b msg -> Svg msg
ladderToSvg drawStep ladder =
    let
        n =
            ladder.rails |> List.length |> toFloat

        adjust i ( name, rail ) =
            rail
                |> scale 0.9 0.9
                |> translate ((i |> toFloat |> (+) 0.5) / n) 0
                |> railToSvg drawStep
    in
        ladder.rails
            |> List.indexedMap adjust
            |> (\xs -> g [] xs)



--HELPERS


{-| provide select and update functions to create a ladder transformer
-}
updateSteps : (a -> Int -> Step b msg -> Bool) -> (a -> Int -> Step b msg -> Step b msg) -> Ladder a b msg -> Ladder a b msg
updateSteps select update =
    select ?? update |> mapStep


{-| simple select and update functions don't need rail info
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


tMatrix : Mat4 -> TypedSvg.Types.Transform
tMatrix mat4 =
    mat4
        |> M4.toRecord
        |> (\m -> TypedSvg.Types.Matrix m.m11 m.m21 m.m12 m.m22 m.m14 m.m24)


tScale : Float -> Float -> TypedSvg.Types.Transform
tScale =
    TypedSvg.Types.Scale


tTranslate : Float -> Float -> TypedSvg.Types.Transform
tTranslate =
    TypedSvg.Types.Translate


fill : Color -> TypedSvg.Core.Attribute msg
fill color =
    SA.fill (TypedSvg.Types.Fill color)


opacity : Float -> TypedSvg.Core.Attribute msg
opacity fraction =
    SA.opacity (TypedSvg.Types.Opacity fraction)



-- MAP/TRANSFORM


mapStep : (a -> Int -> Step b msg -> Step c msg) -> Ladder a b msg -> Ladder a c msg
mapStep f ladder =
    let
        mapRail name =
            mapData (List.indexedMap (f name))
    in
        ladder.rails
            |> List.map (\( name, rail ) -> ( name, mapRail name rail ))
            |> (\x -> { ladder | rails = x })


{-| opportunity for lens
-}
translate :
    Float
    -> Float
    -> { a | transform : Mat4 }
    -> { a | transform : Mat4 }
translate x y =
    mapTransform (M4.translate3 x y 0)


scale : Float -> Float -> { a | transform : Mat4 } -> { a | transform : Mat4 }
scale x y =
    mapTransform (fromCenter (M4.scale3 x y 1))


mapTransform : (a -> b) -> { c | transform : a } -> { c | transform : b }
mapTransform f m =
    { m | transform = f m.transform }


fromCenter : (Mat4 -> Mat4) -> Mat4 -> Mat4
fromCenter f =
    (M4.translate3 -0.5 -0.5 0) << f << (M4.translate3 0.5 0.5 0)


mapData : (a -> b) -> { c | data : a } -> { c | data : b }
mapData f x =
    { x | data = f x.data }


transformChildren :
    { b | transform : Mat4, data : List { a | transform : Mat4 } }
    -> { b | transform : Mat4, data : List { a | transform : Mat4 } }
transformChildren x =
    let
        f =
            mapTransform (M4.mul x.transform)
    in
        { x | data = List.map f x.data }



-- TYPES


type alias WrappedSvg data msg =
    { data : data
    , attributes : List (TypedSvg.Core.Attribute msg)
    , color : Color
    , transform : Mat4
    }


type alias Step data msg =
    WrappedSvg data msg


type alias Rail data msg =
    WrappedSvg (List (Step data msg)) msg


type alias Rung msg =
    WrappedSvg {} msg


type alias Arc =
    { start : Float, stop : Float, color : Color }


type alias StepData =
    { interval : Int, arcs : List Arc }


{-| awkward to store name in tuple list, awkward to modify Rail to store name
-}
type alias Ladder railName data msg =
    { rails : List ( railName, Rail data msg )
    , rungs : Dict ( ( railName, Int ), ( railName, Int ) ) (Rung msg)
    }
