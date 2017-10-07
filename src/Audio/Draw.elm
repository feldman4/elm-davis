module Audio.Draw exposing (..)

import Audio.GL exposing (..)
import Audio.Music exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Color exposing (Color)
import DynamicStyle exposing (hover)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import List.Extra
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3
import Maybe.Extra
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes as SA exposing (fill, noFill, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (percent, px)


type alias WrappedSvg data msg =
    { data : data
    , attributes : List (TypedSvg.Core.Attribute msg)
    , transform : Mat4
    }


type alias Colored a =
    { a | color : Color }


displayChannels : List String -> (String -> msg) -> Html msg
displayChannels names clickRespond =
    let
        nameStyle =
            hover [ ( "color", "blue", "lightblue" ) ]

        makeDiv name =
            div (onClick (clickRespond name) :: nameStyle) [ text name ]
    in
        names
            |> List.map makeDiv
            |> div []


svgScene : List (Svg a) -> Html a
svgScene =
    svg [ SA.width (percent 100), SA.height (percent 100), viewBox 0 0 1 1 ]



-- HELPERS


fill : Color -> TypedSvg.Core.Attribute msg
fill color =
    SA.fill (TypedSvg.Types.Fill color)


opacity : Float -> TypedSvg.Core.Attribute msg
opacity fraction =
    SA.opacity (TypedSvg.Types.Opacity fraction)



-- MAP/transform


{-| opportunity for lens
-}
translate :
    Float
    -> Float
    -> { a | transform : Mat4 }
    -> { a | transform : Mat4 }
translate x y =
    mapTransform (M4.translate3 x y 0)


scale : Float -> Float -> { c | transform : Mat4 } -> { c | transform : Mat4 }
scale x y =
    mapTransform (M4.scale3 x y 1)


scaleFromCenter : Float -> Float -> { a | transform : Mat4 } -> { a | transform : Mat4 }
scaleFromCenter x y =
    mapTransform (fromCenter (M4.scale3 x y 1))


scaleFromCenter3 : Float -> Float -> Float -> Mat4 -> Mat4
scaleFromCenter3 x y z =
    M4.scale3 x y z |> fromCenter


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


transformToAttribute : Mat4 -> TypedSvg.Core.Attribute msg
transformToAttribute mat4 =
    mat4
        |> M4.toRecord
        |> (\m -> TypedSvg.Types.Matrix m.m11 m.m21 m.m12 m.m22 m.m14 m.m24)
        |> (\m -> transform [ m ])



-- useful


horizontalLine : Svg msg
horizontalLine =
    line
        [ SA.x1 (percent 0)
        , SA.x2 (percent 100)
        , SA.y1 (percent 0)
        , SA.y2 (percent 0)
        ]
        []


{-| start and stop in radians, radius in user units
-}
arcCommand : Float -> Float -> Float -> String
arcCommand start stop radius =
    let
        x1 =
            radius * (cos start)

        y1 =
            radius * (sin start)

        x2 =
            radius * (cos stop)

        y2 =
            radius * (sin stop)

        replace s x =
            stringReplace s (x |> toString)
    in
        """
        M x1 y1
        A rx ry x-axis-rotation large-arc-flag sweep-flag x2 y2
        """
            |> replace "rx" radius
            |> replace "ry" radius
            |> replace "x1" x1
            |> replace "y1" y1
            |> replace "x2" x2
            |> replace "y2" y2
            |> replace "x-axis-rotation" 0
            |> replace "large-arc-flag" 0
            |> replace "sweep-flag" 1



-- TEXT


printPossibleChords : List Note -> Html msg
printPossibleChords noteList =
    noteList
        |> List.sort
        |> cons2fromList
        |> Maybe.map notesToChord
        |> Maybe.map allInversions
        |> Maybe.map ((List.map printChord) >> Maybe.Extra.values)
        |> Maybe.withDefault []
        |> List.Extra.unique
        |> String.join ", "
        |> (\x -> div [] [ text x ])



-- GL


{-| for gl
-}
noteToAttr : Note -> Attributes
noteToAttr note =
    let
        { letter, octave } =
            note |> intToNote

        color =
            letter |> letterToColor

        offset =
            (letter |> letterToPosition) |> toFloat |> (*) 0.1

        position =
            V3.vec3 (((octave - 2 |> toFloat) + offset) * 0.25) (((octave - 2 |> toFloat) + offset) * 0.25) 0
                |> (flip V3.add) (V3.vec3 -0.5 -0.5 0)
    in
        { position = position, color = color }


letterToColor : Letter -> V3.Vec3
letterToColor letter =
    case letter of
        A ->
            V3.vec3 0.5 0 0

        A_ ->
            V3.vec3 0 1 0.5

        B ->
            V3.vec3 0 0.5 0

        C ->
            V3.vec3 0 0 0.5

        C_ ->
            V3.vec3 0.5 0 0.5

        D ->
            V3.vec3 0 0.5 0.5

        D_ ->
            V3.vec3 0.5 0.5 0

        E ->
            V3.vec3 0.1 0.2 0.3

        F ->
            V3.vec3 0.3 0.2 0.1

        F_ ->
            V3.vec3 0.9 0.1 0.1

        G ->
            V3.vec3 0.6 0.2 0.4

        G_ ->
            V3.vec3 0.2 0.8 0.2
