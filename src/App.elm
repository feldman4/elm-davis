port module App exposing (Model, Msg(..), init, update, view)

import AnimationFrame
import Audio.Draw exposing (..)
import Audio.GL exposing (..)
import Audio.Ladder exposing (..)
import Audio.Midi exposing (..)
import Audio.Music exposing (middleC)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Audio.Roll exposing (..)
import Cons exposing (cons)
import Html exposing (div, program, text)
import Html.Attributes exposing (..)
import Random exposing (Seed, initialSeed)
import Time exposing (Time)
import WebGL
import Set exposing (Set)


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
    , inputs : Set String
    , selected : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { notes = []
    , time = 0
    , seed = initialSeed 0
    , noteHistory = []
    , inputs = Set.empty
    , selected = Nothing
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
                ( _, nextSeed ) =
                    Random.step Random.bool model.seed

                doIt =
                    model.selected
                        |> Maybe.map ((==) input)
                        |> Maybe.withDefault False
            in
                if doIt then
                    model
                        |> updateNote model.time midiNote
                        |> updateSeed midiNote
                        |> (\x -> x ! [])
                else
                    model ! []

        MIDIInputs midiInputs ->
            case model.selected of
                Just s ->
                    { model | inputs = midiInputs |> Set.fromList } ! []

                Nothing ->
                    ({ model
                        | inputs = midiInputs |> Set.fromList
                        , selected = midiInputs |> List.head
                     }
                    )
                        ! []

        SelectInput name ->
            if Set.member name model.inputs then
                { model | selected = Just name } ! []
            else
                { model | selected = Nothing } ! []

        Animate dt ->
            { model | time = model.time + (dt / 1000) } ! []


updateSeed : MIDINote -> Model -> Model
updateSeed note model =
    model



-- VIEW


view : Model -> Html.Html Msg
view { notes, noteHistory, time, inputs } =
    let
        noteList =
            notes
                |> List.map .note

        noteText =
            noteList
                |> List.map (intToNote >> noteToString)
                |> String.join ","
                |> (++) "notes: "
                |> text
                |> (\x -> div [] [ x ])

        entities =
            notes
                |> List.map .note
                |> List.map noteToAttr
                |> List.map renderCrap

        pianoRoll =
            (notes ++ noteHistory)
                |> List.map (relativeToPresent time)
                |> buildRoll 3
                |> rollToSvg rollNoteToSvg
                |> (\x -> svgScene [ x ])
                |> (\x -> div divAttributes [ x ])

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
            noteHistory
                |> establishRoot
                |> Maybe.withDefault (noteToInt middleC % 12)

        colorNotes c =
            updateStepsSimple (selectNotes root noteList) (colorStep c)

        lightenUnplayedNotes saturation lightness =
            updateStepsSimple
                (selectNotes root noteList >> not)
                ((saturationStep saturation) >> (lightnessStep lightness))

        intervals =
            noteList
                |> List.map (\x -> (x - root) % 12)

        divAttributes =
            [ style [ ( "height", "45%" ), ( "margin", "0 auto" ), ( "display", "block" ) ] ]

        halo =
            case noteList |> Cons.fromList of
                Just xs ->
                    (haloLeading root xs)

                Nothing ->
                    (\_ _ a -> a)

        ladderHtml =
            Audio.Music.bigFour
                |> modesToLadder
                |> mapStep colorByQuality
                |> lightenUnplayedNotes 0.6 0.3
                -- |> updateSteps (\_ _ _ -> True) (haloTriad intervals)
                |>
                    updateSteps (\_ _ _ -> True) halo
                |> ladderToSvg stepToSvg
                |> (\x -> svgScene [ x ])
                |> (\x -> div divAttributes [ x ])
    in
        div [ style [ ( "text-align", "center" ) ] ]
            [ ladderHtml
            , pianoRoll
            , chordText
            , noteText
            , displayChannels (inputs |> Set.toList) SelectInput
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



--     [
--     , Keyboard.downs (keyChange True)
--     , Keyboard.ups (keyChange False)
--     , Window.resizes Resize
--     , Drag.subscriptions DragMsg model.dragModel
--     , Sub.map SpeechMsg (Speech.subscriptions model.speechModel)
--     ]
--         |> Sub.batch
-- PORTS


port midiPort : (( String, ( Int, Int, Int ) ) -> msg) -> Sub msg


port midiInputs : (List String -> msg) -> Sub msg
