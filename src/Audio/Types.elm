module Audio.Types exposing (..)


type alias MIDINote =
    ( Int, Int, Int )


type NotePress a
    = NoteOn a
    | NoteOff a


type alias Note =
    Int


type alias FullNote =
    { letter : Letter, octave : Int }


type alias ChordQuality =
    List Note


type alias NoteEvent =
    { note : Note, start : Float, end : Maybe Float }


type alias NoteModel a =
    { a
        | noteHistory : List NoteEvent
        , notes : List NoteEvent
    }


type Letter
    = A
    | A_
    | B
    | C
    | C_
    | D
    | D_
    | E
    | F
    | F_
    | G
    | G_
