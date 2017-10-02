port module App exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program)
import Html.Attributes exposing (..)
import Time exposing (Time)
import AnimationFrame
import WebGL
import Random exposing (initialSeed, Seed)
import Audio.GL exposing (..)
import Audio.Ladder exposing (..)
import Audio.Midi exposing (..)
import Audio.Music exposing (middleC)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Audio.Visual exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { notes : List NoteEvent
    , noteHistory : List NoteEvent
    , time : Time
    , seed : Seed
    }


init : ( Model, Cmd Msg )
init =
    { notes = []
    , time = 0
    , seed = initialSeed 0
    , noteHistory = []
    }
        ! []



-- UPDATE


type Msg
    = MIDIEvent MIDINote
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MIDIEvent midiNote ->
            let
                ( _, nextSeed ) =
                    Random.step Random.bool model.seed
            in
                model
                    |> updateNote midiNote
                    |> updateSeed midiNote
                    |> (\x -> x ! [])

        Animate dt ->
            { model | time = model.time + (dt / 1000) } ! []


updateNote : MIDINote -> Model -> Model
updateNote midiNote model =
    case midiToNote midiNote of
        Just (NoteOn note) ->
            { model | notes = { note = note, start = model.time, end = Nothing } :: model.notes }

        Just (NoteOff note) ->
            let
                ( thisNote, otherNotes ) =
                    model.notes |> List.partition (strEq note << .note)

                endedNotes =
                    thisNote |> List.map (\n -> { n | end = Just model.time })
            in
                { model
                    | notes = otherNotes
                    , noteHistory = endedNotes ++ model.noteHistory
                }

        Nothing ->
            model


updateSeed : MIDINote -> Model -> Model
updateSeed note model =
    model


strEq : a -> a -> Bool
strEq a a_ =
    (toString a) == (toString a_)



-- VIEW


view : Model -> Html.Html Msg
view { notes, noteHistory, time } =
    let
        noteList =
            notes
                |> List.map .note

        noteText =
            noteList
                |> List.map noteToString
                |> String.join ","
                |> (++) "notes: "
                |> text
                |> (\x -> div [] [ x ])

        entities =
            notes
                |> List.map .note
                |> List.map noteToAttr
                |> List.map renderCrap

        noteHtml =
            (notes ++ noteHistory)
                |> List.map (relativeToPresent time)
                |> svgScale 3
                |> svgScene

        chordText =
            notes |> List.map .note |> printPossibleChords

        glTriangles =
            WebGL.toHtml
                [ width 400
                  -- window.width
                , height 300
                  -- window.height
                , style [ ( "display", "block" ) ]
                ]
                entities

        root =
            noteHistory |> establishRoot |> Maybe.withDefault middleC.letter

        colorNotes c =
            updateStepsSimple (selectNotes root noteList) (rgbStep c)

        lightenUnplayedNotes saturation lightness =
            updateStepsSimple
                (selectNotes root noteList >> not)
                ((saturationStep saturation) >> (lightnessStep lightness))

        ladderHtml =
            Audio.Music.bigFour
                |> modesToLadder
                |> mapStep colorByQuality
                |> lightenUnplayedNotes 0.6 0.8
                |> ladderToSvg
                |> (\x -> svgScene [ x ])
    in
        div []
            [ ladderHtml
            , Html.br [] []
            , noteHtml
            , chordText
            , noteText
            , glTriangles
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ midiPort MIDIEvent
    , AnimationFrame.diffs Animate
    ]
        |> Sub.batch



--     [
--     , Keyboard.downs (keyChange True)
--     , Keyboard.ups (keyChange False)
--     , Window.resizes Resize
--     , Drag.subscriptions DragMsg model.dragModel
--     , Sub.map SpeechMsg (Speech.subscriptions model.speechModel)
--     ]
--         |> Sub.batch
-- PORTS


port midiPort : (( Int, Int, Int ) -> msg) -> Sub msg
