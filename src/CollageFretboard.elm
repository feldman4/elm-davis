module CollageFretboard
    exposing
        ( Grid
        , gridToSvg
        , numberGridDict
        , gridLetterMajor
        , gridLetterMinor
        , gridLetterDiminished
        )

{-| Compose layers. Use natural units and envelope to scale.
Grid of symbols.
-}

import Collage exposing (..)
import Collage.Layout as CL exposing (center, debug, horizontal, stack, vertical)
import Collage.Render exposing (svg)
import Collage.Text as CT exposing (defaultStyle)
import Color
import Dict
import Html
import List.Extra


main : Html.Html msg
main =
    boardWithGrid 6 15 simpleGrid
        |> svg


simpleGrid : Grid msg
simpleGrid =
    let
        a =
            "A-" |> gridLetterMinor
    in
        numberGridDict 6 15
            |> Dict.insert ( 0, 0 ) a


type alias Grid msg =
    Dict.Dict ( Int, Int ) (Collage msg)


gridToSvg : Int -> Int -> Grid msg -> Html.Html msg
gridToSvg strings frets grid =
    (boardWithGrid strings frets grid) |> svg


boardWithGrid : Int -> Int -> Grid msg -> Collage msg
boardWithGrid strings frets grid =
    let
        inlay =
            drawInlays frets
    in
        [ grid |> layoutGridFromDict
        , drawBoard strings frets
        ]
            |> stack
            |> outlineCollage superThinOutlineStyle
            |> center
            |> CL.at CL.bottom (CL.align CL.top inlay)
            |> outlineCollage superThinOutlineStyle
            |> scale 30



-- HELPERS


numberStyle : CT.Style
numberStyle =
    { defaultStyle
        | color = Color.darkGray
        , size = 1
    }


letterStyle : CT.Style
letterStyle =
    { defaultStyle
        | size = 1
        , typeface = CT.Font "Courier New"
        , weight = CT.Bold
    }


aNumber : Int -> CT.Text
aNumber x =
    x
        |> toString
        |> CT.fromString
        |> CT.style numberStyle


aLetter : String -> CT.Text
aLetter x =
    x
        |> CT.fromString
        |> CT.style letterStyle



-- COLLAGE


gridNumber : Int -> Collage msg
gridNumber =
    aNumber >> rendered >> scale 0.4


renderGridLetter : CT.Text -> Collage msg
renderGridLetter =
    rendered >> scale 0.5


gridLetterMajor : String -> Collage msg
gridLetterMajor =
    aLetter >> CT.color (Color.rgb 0 0 255) >> renderGridLetter


gridLetterMinor : String -> Collage msg
gridLetterMinor =
    aLetter >> CT.color (Color.rgb 255 0 0) >> renderGridLetter


gridLetterDiminished : String -> Collage msg
gridLetterDiminished =
    aLetter >> CT.color (Color.rgb 0 120 0) >> renderGridLetter


numberGrid : Int -> Int -> List (List (Collage msg))
numberGrid rows cols =
    List.range 0 cols
        |> List.map gridNumber
        |> List.repeat rows


numberGridDict : Int -> Int -> Grid msg
numberGridDict rows cols =
    List.Extra.lift2 (,) (List.range 0 (cols - 1)) (List.range 0 (rows - 1))
        |> List.map (\( x, y ) -> ( ( x, y ), gridNumber x ))
        |> Dict.fromList


drawBoard : Int -> Int -> Collage msg
drawBoard rows cols =
    let
        pickRect col =
            if List.member col inlayPositions then
                shadedRect
            else
                whiteRect

        oneRow =
            List.range 0 (cols - 1)
                |> List.map pickRect
                |> horizontal

        allRows =
            oneRow
                |> List.repeat rows
                |> vertical
    in
        allRows



-- LAYOUT


layoutGridFromDict : Grid msg -> Collage msg
layoutGridFromDict dict =
    let
        move ( ( x, y ), c ) =
            Collage.shift ( toFloat x, toFloat y |> (*) -1 ) c
    in
        dict |> Dict.toList |> List.map move |> Collage.group


addSpacer : Collage msg -> Collage msg
addSpacer x =
    CL.spacer 1 1 |> CL.impose x


layoutGrid : List (List (Collage msg)) -> Collage msg
layoutGrid xs =
    let
        layoutRow =
            List.map addSpacer >> horizontal
    in
        xs |> List.map layoutRow |> vertical


drawInlays : Int -> Collage msg
drawInlays cols =
    let
        base =
            shadedRect |> scaleX (toFloat cols) |> scaleY 0.8

        drawInlay ( fret, inlayType ) =
            inlayType
                |> pickInlay
                |> center
                |> shift ( 0.75, 0 )
                |> addSpacer
                |> scale 0.4
                |> shift ( toFloat fret, 0 )

        inlayCollage =
            inlays
                |> List.filter (\( i, _ ) -> i < cols)
                |> List.map drawInlay
                |> group
                |> CL.align CL.left
    in
        base |> CL.at CL.left inlayCollage



-- TRANSFORM


outlineCollage : LineStyle -> Collage msg -> Collage msg
outlineCollage lineStyle collage =
    let
        outline =
            rectangle (CL.width collage) (CL.height collage)
                |> Collage.outlined lineStyle
    in
        collage |> CL.at CL.base outline



-- CONSTANTS


thinOutlineStyle : LineStyle
thinOutlineStyle =
    solid verythin (uniform Color.black)


reallyThinOutlineStyle : LineStyle
reallyThinOutlineStyle =
    { defaultLineStyle
        | thickness = 0.08
    }


superThinOutlineStyle :
    { cap : LineCap
    , dashPattern : List ( Int, Int )
    , dashPhase : Int
    , fill : FillStyle
    , join : LineJoin
    , thickness : Float
    }
superThinOutlineStyle =
    { defaultLineStyle
        | thickness = 0.04
    }


shadedRect : Collage msg
shadedRect =
    rectangle 1 1
        |> filled (uniform Color.lightGray)


whiteRect : Collage msg
whiteRect =
    rectangle 1 1
        |> filled (uniform Color.white)


type Inlay
    = Open
    | Single
    | Double


inlays : List ( Int, Inlay )
inlays =
    [ ( 0, Open )
    , ( 3, Single )
    , ( 5, Single )
    , ( 7, Single )
    , ( 9, Single )
    , ( 12, Double )
    , ( 15, Single )
    , ( 17, Single )
    , ( 19, Single )
    , ( 21, Single )
    , ( 24, Double )
    ]


inlayPositions : List Int
inlayPositions =
    let
        keep =
            inlays |> List.map Tuple.first
    in
        List.range 1 24
            |> List.filter ((flip List.member) keep)


pickInlay : Inlay -> Collage msg
pickInlay inlay =
    let
        singleInlay =
            circle 0.2 |> filled (uniform Color.black)
    in
        case inlay of
            Open ->
                circle 0.2 |> outlined reallyThinOutlineStyle

            Single ->
                singleInlay

            Double ->
                horizontal [ singleInlay, CL.spacer 0.2 1, singleInlay ]
