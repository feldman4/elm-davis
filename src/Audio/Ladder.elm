module Audio.Ladder exposing (..)

import Audio.Music exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Color exposing (Color, black, blue, gray, red, rgba)
import Dict exposing (Dict)
import List.Extra
import Math.Matrix4 as M4 exposing (Mat4)
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes as SA exposing (noFill, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (percent, px)


-- BUILD


scalesToLadder : List ( String, Scale ) -> Ladder Int msg
scalesToLadder scales =
    let
        step ( n, y ) =
            { data = n
            , attributes = [ stroke (rgba 255 0 0 0.5) ]
            , transform = M4.identity |> M4.translate3 0 (1 - y) 0
            }

        rail steps =
            { data = steps
            , attributes = []
            , transform = M4.identity
            }

        double xs =
            xs ++ xs

        positions =
            scaleToIntervals >> double >> notePositions

        rootIntervals =
            scaleToIntervals >> double >> (List.scanl (+) 0)

        makeRail scale =
            scale
                |> (\x -> List.Extra.zip (rootIntervals x) (positions x))
                |> List.map step
                |> rail

        rails =
            scales |> List.map (\( a, b ) -> ( a, makeRail b ))
    in
        { rails = rails, rungs = Dict.empty }



-- DRAW


stepToSvg : Step a msg -> Svg msg
stepToSvg step =
    circle
        ([ SA.x (percent 0)
         , SA.y (percent 0)
         , SA.r (percent 2)
         , transform [ step.transform |> tMatrix ]
         , stroke black
         , strokeWidth (percent 1)
         , noFill
         ]
            ++ step.attributes
        )
        []


railToSvg : Rail a msg -> Svg msg
railToSvg rail =
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
                |> List.map stepToSvg
                |> (\xs -> g [] xs)
    in
        g [] [ bar, noteCircles ]


ladderToSvg : Ladder a msg -> Svg msg
ladderToSvg ladder =
    let
        n =
            ladder.rails |> List.length |> toFloat

        adjust i ( name, rail ) =
            rail
                |> scale 0.9 0.9
                |> translate ((i |> toFloat) / n) 0
                |> railToSvg
    in
        ladder.rails
            |> List.indexedMap adjust
            |> (\xs -> g [] xs)



--HELPERS


fillStep : Color -> (Step a msg -> Step a msg)
fillStep color =
    addAttribute (fill color)


addAttribute : a -> { b | attributes : List a } -> { b | attributes : List a }
addAttribute attr x =
    { x | attributes = x.attributes ++ [ attr ] }


selectNotes : Letter -> List Note -> Step Int msg -> Bool
selectNotes root noteList step =
    let
        rootInt =
            noteToInt { letter = root, octave = 0 }

        intervals =
            noteList
                |> List.map noteToInt
                |> List.map (\x -> (x - rootInt) % 12)
    in
        List.member (step.data % 12) intervals


updateSteps : (Step b msg -> Bool) -> (Step b msg -> Step b msg) -> Ladder b msg -> Ladder b msg
updateSteps select update =
    select ?? update |> mapStep


(??) : (a -> Bool) -> (a -> a) -> a -> a
(??) f g x =
    if f x then
        g x
    else
        x


dictMapValues : (a -> b) -> Dict comparable a -> Dict comparable b
dictMapValues f =
    Dict.map (\_ v -> f v)


mapStep : (Step a msg -> Step b msg) -> Ladder a msg -> Ladder b msg
mapStep f ladder =
    let
        mapRail : Rail a msg -> Rail b msg
        mapRail =
            mapData (List.map f)
    in
        { ladder | rails = ladder.rails |> List.map (\( a, b ) -> ( a, mapRail b )) }



-- notePositions : Audio.Music.Scale -> List Float


notePositions : List Int -> List Float
notePositions xs =
    let
        n =
            List.sum xs |> toFloat
    in
        xs
            |> List.map toFloat
            |> List.scanl (+) 0
            |> List.map (\x -> x / n)


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



-- MODIFIERS


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
    , transform : Mat4
    }


type alias Step data msg =
    WrappedSvg data msg


type alias Rail data msg =
    WrappedSvg (List (Step data msg)) msg


type alias Rung msg =
    WrappedSvg {} msg


type alias Ladder data msg =
    { rails : List ( String, Rail data msg )
    , rungs : Dict ( ( String, Int ), ( String, Int ) ) (Rung msg)
    }
