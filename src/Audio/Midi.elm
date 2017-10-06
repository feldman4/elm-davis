module Audio.Midi exposing (..)

import Audio.Types exposing (..)


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
        x1 :: x2 :: x3 :: rest ->
            if (x1 == x2) && (x2 == x3) then
                Just (x1 % 12)
            else
                case rest of
                    [] ->
                        Nothing

                    _ ->
                        rootPattern (List.drop 1 noteList)

        _ ->
            Nothing
