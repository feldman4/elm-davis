module Audio.Midi exposing (..)

import Audio.Types exposing (..)
import Audio.Utility exposing (..)
import Audio.Music exposing (..)


updateNote :
    Float
    -> MIDINote
    -> { c
        | noteHistory : List NoteEvent
        , notes : List NoteEvent
       }
    -> { c
        | noteHistory : List NoteEvent
        , notes : List NoteEvent
       }
updateNote time midiNote model =
    case midiToNote midiNote of
        Just (NoteOn note) ->
            { model | notes = { note = note, start = time, end = Nothing } :: model.notes }

        Just (NoteOff note) ->
            let
                ( thisNote, otherNotes ) =
                    model.notes |> List.partition (strEq note << .note)

                endedNotes =
                    thisNote |> List.map (\n -> { n | end = Just time })
            in
                { model
                    | notes = otherNotes
                    , noteHistory = endedNotes ++ model.noteHistory
                }

        Nothing ->
            model


midiToNote : MIDINote -> Maybe (NotePress Note)
midiToNote ( datatype, intNote, velocity ) =
    case datatype of
        144 ->
            -- note on
            (Just << NoteOn) intNote

        128 ->
            -- note off
            (Just << NoteOff) intNote

        _ ->
            Nothing


{-| notes are sorted from most recent start to earliest start
-}
lastChord : List NoteEvent -> List NoteEvent
lastChord =
    lastChordIter []


lastChordIter : List NoteEvent -> List NoteEvent -> List NoteEvent
lastChordIter soFar noteEvents =
    let
        sortByStart =
            List.sortBy (\n -> -1 * n.start)
    in
        case ( soFar, noteEvents ) of
            ( _, [] ) ->
                soFar

            ( [], noteEvent :: rest ) ->
                case noteEvent.end of
                    Nothing ->
                        lastChordIter [ noteEvent ] rest

                    Just _ ->
                        []

            ( later :: _, earlier :: rest ) ->
                case earlier.end of
                    Nothing ->
                        lastChordIter (sortByStart (earlier :: soFar)) rest

                    Just end ->
                        if end < later.start then
                            soFar
                        else
                            lastChordIter (sortByStart (earlier :: soFar)) rest



{-
   entrance to rabbit hole
   could use a match combinator system
   what patterns do we want to match?
   interval motifs relative to starting note, options to branch etc
   refer to Match.elm
-}


establishRoot : List NoteEvent -> Maybe Note
establishRoot noteEventList =
    Just 0



-- noteEventList |> List.map .note |> rootPattern


rootPattern : List Note -> Maybe Int
rootPattern noteList =
    let
        repeat4 x1 x2 x3 x4 x5 =
            (x1 == x2) && (x1 == x3) && (x1 == x4) && (x1 == x5)

        octave2 x1 =
            x1 < fullNoteToNote { letter = E, octave = 3 }
    in
        case noteList of
            x1 :: x2 :: x3 :: x4 :: x5 :: rest ->
                if
                    repeat4 x1 x2 x3 x4 x5
                    -- && octave2 x1
                then
                    Just (x1 % 12)
                else
                    case rest of
                        [] ->
                            Nothing

                        _ ->
                            rootPattern (List.drop 1 noteList)

            _ ->
                Nothing
