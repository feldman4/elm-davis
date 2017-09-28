port module App exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program)
import Html.Attributes exposing (..)
import Time exposing (Time)
import AnimationFrame
import GL exposing (..)
import Audio.Types exposing (..)
import WebGL
import Math.Vector3 exposing (vec3, Vec3)
import Audio.Music exposing (..)
import Random exposing (initialSeed, Seed)


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


type alias MIDINote =
    ( Int, Int, Int )


init : ( Model, Cmd Msg )
init =
    { notes = []
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
        MIDIEvent ( datatype, intNote, volume ) ->
            let
                ( _, nextSeed ) =
                    Random.step Random.bool model.seed
            in
                case datatype of
                    144 ->
                        -- note on
                        { model | notes = (intNote |> intToNote) :: model.notes, seed = nextSeed }
                            ! []

                    128 ->
                        -- note off
                        { model | notes = [] }
                            ! []

                    _ ->
                        model ! []

        Animate dt ->
            { model | time = model.time + dt } ! []


intToNote : Int -> Note
intToNote intNote =
    let
        letters =
            [ C, C_, D, D_, E, F, F_, G, G_, A, A_, B ]
    in
        { letter =
            letters
                |> List.drop (intNote % 12)
                |> List.head
                |> Maybe.withDefault A
        , octave = (floor ((intNote |> toFloat) / 12)) - 1
        }


noteToString : Note -> String
noteToString note =
    let
        noteSymbol =
            case note.letter of
                A ->
                    "A"

                A_ ->
                    "A#"

                B ->
                    "B"

                C ->
                    "C"

                C_ ->
                    "C#"

                D ->
                    "D"

                D_ ->
                    "D#"

                E ->
                    "E"

                F ->
                    "F"

                F_ ->
                    "F#"

                G ->
                    "G"

                G_ ->
                    "G#"
    in
        noteSymbol ++ (note.octave |> toString)



-- VIEW


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
