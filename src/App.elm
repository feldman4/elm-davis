port module App exposing (Model, Msg(..), init, update, view)

import AnimationFrame
import Audio.Component exposing (..)
import Audio.Draw exposing (svgScene, printPossibleChords)
import Audio.Midi exposing (..)
import Audio.Types exposing (..)
import Audio.Widgets exposing (divAttributes)
import Dict exposing (Dict)
import Html exposing (div, program, text)
import Html.Attributes exposing (style)
import Keyboard.Extra as KE exposing (Key)
import Time exposing (Time)
import FoldingTree


-- MODEL


initComponents : Model -> ( Config Key, Selection (Component Msg) )
initComponents ({ noteHistory, time, inputs } as model) =
    let
        displayChannels configs =
            Audio.Draw.displayChannels (inputs |> Dict.toList) SelectInput
    in
        [ { name = "ladderHtml"
          , options = [ ( KE.CharT, [ "triad", "dots" ] ), ( KE.CharS, [ "guitarSustain", "pianoSustain", "noSustain" ] ) ]
          , component = Audio.Widgets.ladderHtml model "ladderHtml.guitarSustain" "ladderHtml.pianoSustain" "ladderHtml.triad"
          , on = True
          }
        , { name = "pianoRoll"
          , options = []
          , component = Audio.Widgets.pianoRoll model
          , on = False
          }
        , { name = "notesText"
          , options = []
          , component = Audio.Widgets.notesToText model
          , on = True
          }
        , { name = "chordText"
          , options = []
          , component = Audio.Widgets.chordText model "ladderHtml.guitarSustain" "ladderHtml.pianoSustain"
          , on = True
          }
        , { name = "guitarVoicings"
          , options = []
          , component = Audio.Widgets.guitarVoicings model "ladderHtml.guitarSustain" "ladderHtml.pianoSustain"
          , on = True
          }
        , { name = "channels"
          , options = []
          , component = displayChannels
          , on = False
          }
        ]
            |> reshapeComponents


makeConfigTree : a -> FoldingTree.Tree ( Bool, String )
makeConfigTree =
    (\x -> "[" ++ toString x ++ "]")
        >> FoldingTree.toStringTree
        >> FoldingTree.unfoldedTree


init : ( Model, Cmd Msg )
init =
    let
        finish model =
            let
                ( config, components ) =
                    initComponents model
            in
                { model
                    | config = config
                    , components = components
                    , configTree = makeConfigTree config
                }
                    ! []
    in
        finish
            { notes = []
            , time = 0
            , noteHistory = []
            , inputs = Dict.empty
            , config = Dict.empty
            , configTree = FoldingTree.Node ( False, "" ) []
            , components = []
            , buffer = []
            }


type alias Model =
    { notes : List NoteEvent
    , noteHistory : List NoteEvent
    , time : Time
    , inputs : Dict String Bool
    , components : Selection (Component Msg)
    , config : Config Key
    , configTree : FoldingTree.Tree ( Bool, String )
    , buffer : List Key
    }



-- UPDATE


type Msg
    = MIDIEvent ( String, MIDINote )
    | MIDIInputs (List String)
    | SelectInput String
    | Animate Time
    | KeyPress Key
    | FoldingTreeMsg FoldingTree.Msg


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

        KeyPress key ->
            { model
                | config = updateAllConfig key model.config
                , components = toggleSelectionNumKey key model.components
                , buffer = key :: model.buffer
            }
                ! []

        FoldingTreeMsg msg ->
            { model | configTree = FoldingTree.update msg model.configTree } ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        ( _, components ) =
            initComponents model

        selectedComponents =
            components
                |> copySelection model.components
                |> selected

        overlay =
            makeConfigTree model.config
                |> FoldingTree.copyFoldState model.configTree
                |> FoldingTree.viewTreeDepth FoldingTree.draw3
                |> Html.map FoldingTreeMsg
                |> (\x -> [ x ])
                |> div [ style [ ( "position", "absolute" ), ( "bottom", "0%" ), ( "border", "black solid 2px" ) ] ]

        finalDiv x =
            let
                quoteCount =
                    model.buffer |> List.filter ((==) KE.BackQuote) |> List.length
            in
                case quoteCount % 2 of
                    1 ->
                        div [] [ x, overlay ]

                    _ ->
                        x
    in
        viewComponents model.config selectedComponents
            |> div [ style [ ( "text-align", "center" ) ] ]
            |> finalDiv



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ midiPort MIDIEvent
    , midiInputs MIDIInputs
    , AnimationFrame.diffs Animate
    , KE.downs KeyPress
    ]
        |> Sub.batch



-- PORTS


port midiPort : (( String, ( Int, Int, Int ) ) -> msg) -> Sub msg


port midiInputs : (List String -> msg) -> Sub msg



-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
