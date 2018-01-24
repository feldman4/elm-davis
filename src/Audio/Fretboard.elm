module Audio.Fretboard exposing (..)

import Audio.Draw exposing (..)
import Audio.Fretted exposing (Grip, filterMaybeSix, exampleGrips)
import Color exposing (Color, black, gray, red, rgba)
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Core exposing (Svg)
import Math.Matrix4 as M4
import Dict exposing (Dict)
import TypedSvg exposing (circle, g, line, rect, svg, text_)
import TypedSvg.Attributes as SA exposing (stroke, strokeWidth, viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types as ST exposing (percent, px)
import Html exposing (div, br)
import Html.Attributes exposing (style)


divAttributes : List (Html.Attribute msg)
divAttributes =
    [ style
        [ ( "width", "25%" )
        , ( "height", "100 px" )
        , ( "margin", "0 auto" )
        , ( "display", "block" )
        , ( "flex-grow", "0" )
        ]
    ]


flexParent : List (Html.Attribute msg)
flexParent =
    [ style
        [ ( "display", "flex" )
        , ( "flex-flow", "row wrap" )
        , ( "justify-content", "flex-start" )
        , ( "align-items", "flex-start" )
        , ( "width", "70%" )
        , ( "margin", "0 auto" )
        ]
    ]


main : Html.Html msg
main =
    let
        coloredExample =
            exampleFretboard
                |> fretboardToSvg drawFret
                |> svgScene
                |> (\x -> div divAttributes [ x ])
                |> (\x -> div flexParent (List.repeat 4 x))

        blackExample =
            exampleGrips
                |> List.take 4
                |> drawGrips
    in
        -- coloredExample
        blackExample


svgFixed : Svg a -> Html.Html a
svgFixed x =
    svg [ SA.width (px 100), SA.height (px 100), viewBox 0 0 1 1 ] [ x ]


drawGrips : List Grip -> Html.Html msg
drawGrips grips =
    grips
        |> List.map blackFretboard
        |> List.map (fretboardToSvg drawFret)
        |> List.map svgFixed
        |> List.map (\x -> div divAttributes [ x ])
        |> div flexParent


blackFretboard : Grip -> Fretboard msg
blackFretboard grip =
    grip
        |> filterMaybeSix
        |> List.map (\( string, fret ) -> ( string, fret, rgba 0 0 0 1 ))
        |> buildFretboard


exampleFretboard : Fretboard msg
exampleFretboard =
    [ ( 0, 6, rgba 120 30 30 1 )
    , ( 1, 5, rgba 0 127 0 1 )
    , ( 2, 3, rgba 200 100 0 1 )
    , ( 3, 3, rgba 0 127 0 1 )
    , ( 4, 4, rgba 50 50 150 1 )
    , ( 5, 0, rgba 70 70 70 1 )
    ]
        |> buildFretboard


drawFret : Fret msg -> Svg msg
drawFret { data } =
    let
        color =
            data
    in
        circle
            [ SA.r (percent 100)
            , strokeWidth (percent 2)
            , stroke color
            , SA.cx (percent 0)
            , SA.cy (percent 0)
            , fill color
            ]
            []


{-|
Draw a fretboard
- optional symbol at each fret
- numbers below frets
- could draw bar across frets (or just make out of individual svg)
- width to show?
-}
buildFretboard : List ( Int, Int, Color ) -> Fretboard msg
buildFretboard frets =
    let
        f ( string, fret, data ) =
            Dict.insert ( string, fret ) (defaultFret data)
    in
        frets
            |> List.foldl f Dict.empty


defaultFret : FretData -> Fret msg
defaultFret data =
    { data = data, transform = M4.identity, attributes = [] }


fretboardToSvg : (Fret msg -> Svg msg) -> Fretboard msg -> Svg msg
fretboardToSvg drawFret fretboard =
    let
        numStrings =
            5

        numFrets =
            fretboard
                |> Dict.keys
                |> List.map (\( _, b ) -> b)
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 6

        lowestFretted =
            fretboard
                |> Dict.keys
                |> List.map (\( _, b ) -> b)
                |> List.filter ((/=) 0)
                |> List.minimum
                |> Maybe.withDefault 1

        fretsOnString i =
            fretboard
                |> Dict.toList
                |> List.filter (\( ( j, _ ), v ) -> j == i)
                |> List.map (\( ( _, k ), v ) -> ( reduceFret k, v ))

        reduceFret i =
            if i == 0 then
                0
            else
                i - lowestFretted + 1

        yOffset i =
            let
                transform =
                    M4.identity
                        |> scaleFromCenter3 0.9 0.9 1
                        |> M4.translate3 0 (lineSpacing 0.2 numStrings i) 0
            in
                gTransform transform

        annotate s x =
            let
                transform =
                    M4.identity |> scaleFromCenter3 0.85 0.85 1 |> M4.translate3 0 -0.1 0

                letterTransform =
                    M4.identity |> M4.translate3 0.55 0.9 0 |> M4.scale3 0.17 0.17 1

                letter =
                    text_ [ SA.textAnchor ST.AnchorMiddle, SA.fontSize (percent 8) ] [ text s ]
            in
                g [] [ gTransform transform x, gTransform letterTransform letter ]
    in
        List.range 0 numStrings
            |> List.map fretsOnString
            |> List.map (drawString drawFret (reduceFret numFrets))
            |> List.indexedMap yOffset
            |> g [ transformToAttribute (M4.identity |> scaleFromCenter3 1 -1 1) ]
            |> annotate (lowestFretted |> toString)


gTransform : M4.Mat4 -> Svg msg -> Svg msg
gTransform transform x =
    g [ transformToAttribute transform ] [ x ]


{-| Place fret svg along string. Fret value of 0 is always aligned to left
of string. Could break string line at physical fret position, between dots
-}
drawString : (Fret msg -> Svg msg) -> Int -> List ( Int, Fret msg ) -> Svg msg
drawString drawFret numFrets frets =
    let
        string =
            g [ stroke black, strokeWidth (percent 2) ] [ horizontalLine ]
                |> gTransform (M4.identity |> M4.translate3 0.065 0 0)

        xOffset i =
            let
                x =
                    if i == 0 then
                        0
                    else
                        -- (lineSpacing 0.2 (numFrets - 1) (i - 1)) + 0.1
                        (lineSpacing 0.1 4 (i - 1)) + 0.1
            in
                M4.identity
                    |> M4.translate3 x 0 0
                    |> M4.scale3 0.05 0.05 1

        drawFret2 ( i, fret ) =
            gTransform (xOffset i) (drawFret fret)
    in
        frets
            |> List.map drawFret2
            |> (\xs -> g [] (string :: xs))


lineSpacing : Float -> Int -> Int -> Float
lineSpacing margin n i =
    (toFloat i / toFloat (n - 1)) * (1 - 2 * margin) + margin


type alias Fretboard msg =
    Dict ( Int, Int ) (Fret msg)


type alias Fret msg =
    WrappedSvg FretData msg


type alias FretData =
    Color
