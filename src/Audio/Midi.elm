module Audio.Midi exposing (..)

import Audio.Types exposing (..)
import Audio.Utility exposing (..)


midiToNote : MIDINote -> Maybe (NotePress Note)
midiToNote ( datatype, intNote, velocity ) =
    case datatype of
        144 ->
            -- note on
            (Just << NoteOn) (intNote |> intToNote)

        128 ->
            -- note off
            (Just << NoteOff) (intNote |> intToNote)

        _ ->
            Nothing



{-
   entrance to rabbit hole
   could use a match combinator system
   what patterns do we want to match?
   interval motifs relative to starting note, options to branch etc
   refer to Match.elm
-}


establishRoot : List NoteEvent -> Maybe Letter
establishRoot noteEventList =
    noteEventList |> List.map .note |> rootPattern


rootPattern : List Note -> Maybe Letter
rootPattern noteList =
    case noteList of
        x1 :: x2 :: x3 :: rest ->
            if (x1 == x2) && (x2 == x3) then
                Just x1.letter
            else
                case rest of
                    [] ->
                        Nothing

                    _ ->
                        rootPattern (List.drop 1 noteList)

        _ ->
            Nothing
