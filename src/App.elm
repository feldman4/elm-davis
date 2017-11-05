port module App exposing (Model, Msg(..), init, update, view)

import AnimationFrame
import Audio.Draw exposing (..)
import Audio.GL exposing (..)
import Audio.Ladder exposing (..)
import Audio.Midi exposing (..)
import Audio.Music exposing (..)
import Audio.Roll exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Cons exposing (cons)
import Dict exposing (Dict)
import Html exposing (div, program, text)
import Html.Attributes exposing (..)
import List.Extra
import Time exposing (Time)


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
    , inputs : Dict String Bool
    }


init : ( Model, Cmd Msg )
init =
    { notes = []
    , time = 0
    , noteHistory = []
    , inputs = Dict.empty
    }
        ! []



-- UPDATE


type Msg
    = MIDIEvent ( String, MIDINote )
    | MIDIInputs (List String)
    | SelectInput String
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MIDIEvent ( input, midiNote ) ->
            let
                selected =
                    model.inputs
                        |> Dict.toList
                        |> List.filter (\( a, b ) -> b)
                        |> List.map (\( a, b ) -> a)
            in
                if List.member input selected then
                    model
                        |> updateNote model.time midiNote
                        |> (\x -> x ! [])
                else
                    model ! []

        MIDIInputs midiInputs ->
            { model | inputs = List.foldl (\k d -> Dict.insert k True d) model.inputs midiInputs } ! []

        SelectInput name ->
            let
                toggle value =
                    case value of
                        Just bool ->
                            Just (not bool)

                        Nothing ->
                            Nothing
            in
                { model | inputs = model.inputs |> Dict.update name toggle, notes = [] } ! []

        Animate dt ->
            { model | time = model.time + (dt / 1000) } ! []



-- VIEW


view : Model -> Html.Html Msg
view ({ noteHistory, time, inputs } as model) =
    let
        notes =
            model.notes
                |> List.map .note

        chord =
            (model.notes ++ noteHistory)
                |> lastChord
                |> List.map .note
                |> List.Extra.unique

        notesToText label notes_ =
            notes_
                |> List.map (noteToFullNote >> printFullNote)
                |> String.join ","
                |> (++) label
                |> text
                |> (\x -> div [] [ x ])

        glTriangles =
            chord
                |> List.map noteToAttr
                |> List.map renderCrap
                |> entitiesToHtml

        pianoRoll =
            (model.notes ++ noteHistory)
                |> List.map (relativeToPresent time)
                |> buildRoll 3
                |> rollToSvg rollNoteToSvg
                |> (\x -> svgScene [ x ])
                |> (\x -> div divAttributes [ x ])

        chordText =
            chord |> printPossibleChords

        root =
            noteHistory
                |> establishRoot
                |> Maybe.withDefault (fullNoteToNote middleC % 12)

        colorNotes c =
            updateStepsSimple (selectNotes root chord) (colorStep c)

        lightenUnplayedNotes saturation lightness =
            updateStepsSimple
                (selectNotes root chord >> not)
                ((saturationStep saturation) >> (lightnessStep lightness))

        intervals =
            notes
                |> List.map (\x -> (x - root) % 12)

        divAttributes =
            [ style [ ( "height", "45%" ), ( "margin", "0 auto" ), ( "display", "block" ) ] ]

        halo =
            case chord |> Cons.fromList of
                Just xs ->
                    (haloLeading root xs)

                Nothing ->
                    (\_ _ a -> a)

        ladderHtml =
            Audio.Music.bigFour
                |> modesToLadder
                |> mapStep colorByQuality
                |> lightenUnplayedNotes 0.6 0.3
                |> updateSteps (\_ _ _ -> True) halo
                -- |> updateSteps (\_ _ _ -> True) (haloTriad intervals)
                |>
                    ladderToSvg stepToSvg
                |> (\x -> svgScene [ x ])
                |> (\x -> div divAttributes [ x ])
    in
        div [ style [ ( "text-align", "center" ) ] ]
            [ ladderHtml
            , pianoRoll
            , displayChannels (inputs |> Dict.toList) SelectInput
            , chordText
            , notesToText "notes: " notes
            , notesToText "chord notes: " chord
              -- , glTriangles
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ midiPort MIDIEvent
    , midiInputs MIDIInputs
    , AnimationFrame.diffs Animate
    ]
        |> Sub.batch



-- PORTS


port midiPort : (( String, ( Int, Int, Int ) ) -> msg) -> Sub msg


port midiInputs : (List String -> msg) -> Sub msg
