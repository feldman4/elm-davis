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
