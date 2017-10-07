module Audio.Midi exposing (..)

import Audio.Types exposing (..)
import Audio.Utility exposing (..)


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



{-
   entrance to rabbit hole
   could use a match combinator system
   what patterns do we want to match?
   interval motifs relative to starting note, options to branch etc
   refer to Match.elm
-}


establishRoot : List NoteEvent -> Maybe Note
establishRoot noteEventList =
    -- Just 0
    noteEventList |> List.map .note |> rootPattern


rootPattern : List Note -> Maybe Int
rootPattern noteList =
    case noteList of
        x1 :: x2 :: x3 :: x4 :: x5 :: rest ->
            if (x1 == x2) && (x1 == x3) && (x1 == x4) && (x1 == x5) then
                -- if (x4 - x5 == 0) && (x3 - x4 == 2) && (x2 - x3 == 2) && ((x1 - x2 |> Debug.log "d") == 1) then
                Just (x1 % 12 |> Debug.log "")
            else
                case rest of
                    [] ->
                        Nothing

                    _ ->
                        rootPattern (List.drop 1 noteList)

        _ ->
            Nothing
