module Audio.Leading exposing (..)

import Audio.Music exposing (..)
import Audio.Utility exposing (..)
import Cons exposing (Cons, cons)


{- Create arcs indicating voice leading into the 1-3-5 at this step.

   Position the arcs according to the currently played tone order (low to high).
   Only lead with bottom 3 tones. When 1 tone is played, triple it to get low and
   high leading. When 2 tones are played, double the higher or lower depending on
   range. When nothing is played, show nothing.

   Indicate the amount of voice leading using left- or right-aligned dots for
   upward and downward voice leading. Also try showing hollow dots for downward leading.

   Use a solid bar if the voice matches.

   Hide for the detected root? What to show if a triad is not being played?

   1. show voices in order lowest to highest
   - need to pick 3, could remove octave doublings and go with lowest
     - screws up common voicings like Maj7 rooted on A (1-5-7-3)
     - just support 4, covers most guitar voicings. show nothing for the unused
     voice (can extend later for 7th chords)

   2. show voices in order of role in current chord (i.e., remove inversion)

   Steps:
   1. for each of 1-3-5 at this step, is it in played notes? if so, remove both
   and set the voice leading to zero
   2. for remaining 1-3-5 x played notes, do linear assignment with cost proportional
   to size of voice leading. brute force scales as (n choose k) where n = played notes
   and k = remaining 1-3-5

   (5 3) = 5! / (2! * 3!) = 10
   not too bad

   (9 3) = 9 *  8 * 7 / 6 = 3 * 4 * 7 = 84
   worst case, could cache if needed

-}


findLeading : Cons Int -> Mode -> Int -> List (List ( Int, Int ))
findLeading intervalsPlayed mode intervalStep =
    let
        intervalsUsed =
            case intervalsPlayed |> Cons.sort |> Cons.uncons of
                ( a, b :: c :: rest ) ->
                    [ a, b, c ]

                ( a, [ b ] ) ->
                    [ a, b, b ]

                ( a, [] ) ->
                    [ a, a, a ]

        lowest =
            intervalsUsed |> List.head |> Maybe.withDefault 0

        first =
            intervalsUsed
                |> cons2fromList
                |> Maybe.map notesToChord

        second : Maybe (Rooted Chord)
        second =
            mode
                |> modeToChord (cons 2 [ 4 ])
                |> rootChord intervalStep
                |> chordToNotes
                |> Cons.toList
                |> List.map (circleClamp (lowest - 12) (lowest))
                |> cons2fromList
                |> Maybe.map notesToChord
    in
        second
            |> (Maybe.map2 leadingWithInversions) first
            |> Maybe.withDefault []