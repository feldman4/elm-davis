port module App exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program)
import Html.Attributes exposing (..)
import Time exposing (Time)
import AnimationFrame
import WebGL
import Random exposing (initialSeed, Seed)
import Audio.Types exposing (..)
import Audio.Music exposing (..)
import Audio.Midi exposing (..)
import Audio.GL exposing (..)
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
    { notes : List Note
    , time : Time
    , seed : Seed
    }


init : ( Model, Cmd Msg )
init =
    { notes = [ { letter = A, octave = 2 }, { letter = B, octave = 1 } ]
    , time = 3
    , seed = initialSeed 0
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
            { model | time = model.time + dt } ! []


updateNote : MIDINote -> Model -> Model
updateNote midiNote model =
    case midiToNote midiNote of
        Just (NoteOn note) ->
            { model | notes = note :: model.notes }

        Just (NoteOff note) ->
            { model | notes = model.notes |> List.filter (not << strEq note) }

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
view model =
    let
        noteList =
            div [] [ model.notes |> List.map noteToString |> String.join "," |> text ]

        dummyNotes =
            [ { letter = A, octave = 2 } ]

        entities =
            model.notes
                |> List.map noteToAttr
                |> List.map renderCrap

        noteSvg =
            svgScene (svgScale [])

        -- dummyNotes |> List.map noteToVec |> List.map renderCrap
    in
        div []
            [ WebGL.toHtml
                [ width 400
                  -- window.width
                , height 300
                  -- window.height
                , style [ ( "display", "block" ) ]
                ]
                entities
            , noteList
            , noteSvg
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
