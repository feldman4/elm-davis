module Audio.Midi exposing (..)

import Audio.Types exposing (..)


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


type NotePress a
    = NoteOn a
    | NoteOff a


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
