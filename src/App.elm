port module App exposing (Model, Msg(..), init, update, view)

import AnimationFrame
import Audio.Component exposing (..)
import Audio.Draw exposing (svgScene, printPossibleChords)
import Audio.Fretboard exposing (drawGrips)
import Audio.Fretted exposing (chordToGrips, difficulty, standardE)
import Audio.Ladder exposing (..)
import Audio.Midi exposing (..)
import Audio.Music exposing (..)
import Audio.Roll exposing (..)
import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Cons exposing (cons)
import Dict exposing (Dict)
import Html exposing (div, program, text)
import Html.Attributes exposing (style)
import Keyboard.Extra as KE exposing (Key)
import List.Extra
import Time exposing (Time)
import FoldingTree


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
    , components : Selection (Component Msg)
    , config : Config Key
    , configTree : FoldingTree.Tree ( Bool, String )
    , buffer : List Key
    }


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


divAttributes : List (Html.Attribute msg)
divAttributes =
    [ style [ ( "height", "45%" ), ( "margin", "0 auto" ), ( "display", "block" ) ] ]


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


makeConfigTree : a -> FoldingTree.Tree ( Bool, String )
makeConfigTree =
    (\x -> "[" ++ toString x ++ "]")
        >> FoldingTree.toStringTree
        >> FoldingTree.unfoldedTree


initComponents : Model -> ( Config Key, Selection (Component Msg) )
initComponents ({ noteHistory, time, inputs } as model) =
    let
        notes =
            model.notes
                |> List.map .note

        chordPiano =
            (model.notes ++ noteHistory)
                |> lastChordPiano
                |> List.map .note
                |> List.Extra.unique

        chordGuitar =
            (model.notes ++ noteHistory)
                |> lastChordPiano
                |> List.map .note
                |> List.Extra.unique

        chord guitarFlag pianoFlag configs =
            if List.member guitarFlag configs then
                chordGuitar
            else if List.member pianoFlag configs then
                chordPiano
            else
                notes |> List.Extra.unique

        root =
            noteHistory
                |> establishRoot
                |> Maybe.withDefault (fullNoteToNote middleC % 12)

        halo chord =
            case chord |> Cons.fromList of
                Just xs ->
                    (haloLeading root xs)

                Nothing ->
                    (\_ _ a -> a)

        lightenUnplayedNotes chord saturation lightness =
            updateStepsSimple
                (selectNotes root chord >> not)
                ((saturationStep saturation) >> (lightnessStep lightness))

        ladderHtml guitarFlag pianoFlag triadFlag configs =
            let
                chord_ =
                    (chord guitarFlag pianoFlag configs)

                update =
                    if List.member triadFlag configs then
                        haloTriad (notes |> List.map (\x -> (x - root) % 12))
                    else
                        halo chord_
            in
                Audio.Music.bigFour
                    |> modesToLadder
                    |> mapStep colorByQuality
                    |> lightenUnplayedNotes chord_ 0.6 0.3
                    |> updateSteps (\_ _ _ -> True) update
                    |> ladderToSvg stepToSvg
                    |> svgScene
                    |> (\x -> div divAttributes [ x ])

        pianoRoll configs =
            (model.notes ++ noteHistory)
                |> List.map (relativeToPresent time)
                |> buildRoll 3
                |> rollToSvg rollNoteToSvg
                |> svgScene
                |> (\x -> div divAttributes [ x ])

        chordText guitarFlag pianoFlag configs =
            let
                chordNames =
                    chord guitarFlag pianoFlag configs
                        |> printPossibleChords
            in
                case chordNames of
                    [] ->
                        div [] [ text "no chord" ]

                    _ ->
                        chordNames
                            |> String.join ", "
                            |> (\x -> div [] [ text x ])

        displayChannels configs =
            Audio.Draw.displayChannels (inputs |> Dict.toList) SelectInput

        guitarVoicings guitarFlag pianoFlag configs =
            (chord guitarFlag pianoFlag configs)
                |> List.take 4
                |> cons2fromList
                |> Maybe.map notesToChord
                |> Maybe.map (allInversionsWindow 5)
                |> Maybe.map (List.concatMap (chordToGrips standardE))
                |> Maybe.withDefault []
                |> List.sortBy difficulty
                |> List.take 8
                |> drawGrips

        notesToText configs =
            notes
                |> List.map (noteToFullNote >> printFullNote)
                |> String.join " "
                |> (\s ->
                        if String.length s == 0 then
                            "no notes"
                        else
                            s
                   )
                |> text
                |> (\x -> div [] [ x ])
    in
        [ { name = "ladderHtml"
          , options = [ ( KE.CharT, [ "triad", "dots" ] ), ( KE.CharS, [ "guitarSustain", "pianoSustain", "noSustain" ] ) ]
          , component = ladderHtml "ladderHtml.guitarSustain" "ladderHtml.pianoSustain" "ladderHtml.triad"
          , on = True
          }
        , { name = "pianoRoll"
          , options = []
          , component = pianoRoll
          , on = False
          }
        , { name = "notesText"
          , options = []
          , component = notesToText
          , on = True
          }
        , { name = "chordText"
          , options = []
          , component = chordText "ladderHtml.guitarSustain" "ladderHtml.pianoSustain"
          , on = True
          }
        , { name = "guitarVoicings"
          , options = []
          , component = guitarVoicings "ladderHtml.guitarSustain" "ladderHtml.pianoSustain"
          , on = True
          }
        , { name = "channels"
          , options = []
          , component = displayChannels
          , on = False
          }
        ]
            |> reshapeComponents


reshapeComponents :
    List { c | component : a, name : comparable, on : b, options : v }
    -> ( Dict comparable v, List ( b, a ) )
reshapeComponents xs =
    let
        f { name, options } =
            ( name, options )

        g { on, component } =
            ( on, component )
    in
        ( xs |> List.map f |> Dict.fromList, xs |> List.map g )



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
