module Audio.Types exposing (..)


type alias Note =
    { letter : Letter, octave : Int }


type alias Chord =
    List Note


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
