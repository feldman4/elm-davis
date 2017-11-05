module Audio.Leading exposing (..)

import Audio.Music exposing (..)
import Audio.Utility exposing (..)
import Cons exposing (Cons, cons)
import List.Extra


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


{-| Return pairs of intervals.


Need to search inversions of second chord.


-}
leading : Rooted Chord -> Rooted Chord -> List (List ( Int, Int ))
leading first second =
    let
        a =
            first |> chordToNotes |> Cons.toList

        b =
            second |> chordToNotes |> Cons.toList

        k =
            List.length a

        cost ( x, y ) =
            abs (x - y)
    in
        b
            |> permutations k
            |> List.map (List.Extra.zip a)
            |> List.sortBy (List.map cost >> List.sum)


leadingWithInversions2 :
    Rooted Chord
    -> Rooted Chord
    -> List (List ( Int, Int ))
leadingWithInversions2 first second =
    let
        cost ( x, y ) =
            abs (x - y)

        firstNotes =
            first |> chordToNotes

        bottom =
            firstNotes |> Cons.head

        top =
            firstNotes |> Cons.reverse |> Cons.head

        window =
            5

        seconds =
            allInversions2 (bottom - window) (top + window) second
    in
        seconds
            |> List.concatMap (leading first)
            |> List.sortBy (List.map cost >> List.sum)


leadingWithInversions : Rooted Chord -> Rooted Chord -> List (List ( Int, Int ))
leadingWithInversions first second =
    let
        cost ( x, y ) =
            abs (x - y)

        shift y =
            { second | root = second.root |> (\x -> x + (12 * y)) }

        seconds =
            [ -2, -1, 0, 1, 2, 3, 4 ]
                |> List.concatMap (\x -> allInversions (shift x))
    in
        seconds
            |> List.concatMap (leading first)
            |> List.sortBy (List.map cost >> List.sum)


{-| -}
findLeading : Rooted Chord -> Rooted Mode -> List (List ( Int, Int ))
findLeading firstChord targetMode =
    let
        numNotesInChord =
            firstChord |> chordToNotes |> Cons.length
    in
        if numNotesInChord /= 3 then
            []
        else
            { scale = targetMode.scale, mode = targetMode.mode }
                |> modeToChord (cons 2 [ 4 ])
                |> rootChord targetMode.root
                |> leadingWithInversions2 firstChord
