module Parse exposing (..)

import Html
import Html exposing (text, div, textarea)
import Maybe.Extra


main : Html.Html msg
main =
    Html.text ""



-- TYPES


type Tree a
    = Branch (List (Tree a))
    | Leaf a


type alias ParseState =
    { branches : List (Tree String)
    , done : Maybe (Tree String)
    }


type alias Operand =
    Symbol


type alias Symbol =
    String



-- PARSE


type alias ParseConfig =
    { classifyParen : String -> Paren }


type Paren
    = OpenParen
    | ClosedParen
    | NotAParen


elmPrintParen : String -> Paren
elmPrintParen s =
    case s of
        "(" ->
            OpenParen

        ")" ->
            ClosedParen

        "[" ->
            OpenParen

        "]" ->
            ClosedParen

        "{" ->
            OpenParen

        "}" ->
            ClosedParen

        _ ->
            NotAParen


schemePrintParen : String -> Paren
schemePrintParen s =
    case s of
        "(" ->
            OpenParen

        ")" ->
            ClosedParen

        _ ->
            NotAParen


elmPrintConfig : { classifyParen : String -> Paren }
elmPrintConfig =
    { classifyParen = elmPrintParen }


schemeConfig : { classifyParen : String -> Paren }
schemeConfig =
    { classifyParen = schemePrintParen }


{-|
1. parse a list expression by folding over the tokens a function
ParseState -> String -> ParseState. If it encounters a regular symbol, it
appends it to the head of ParseState.branches. If it encounters an open paren,
it makes a new head in ParseState.branches. If it encounters a close paren, it
wraps the head of ParseState.branches in a Branch tag, pops it off, and appends it
to the next branch. Don't need to explicitly track parenthesis depth. If we encounter
an extra close paren, ParseState.branches is empty and we Nothing out. If
parentheses are matched, we end with exactly one item in ParseState.branches
-}
parseToken : ParseConfig -> String -> ParseState -> Maybe ParseState
parseToken config token parseState =
    case parseState.done of
        Just _ ->
            Nothing

        Nothing ->
            case config.classifyParen token of
                OpenParen ->
                    Just { parseState | branches = (Branch []) :: parseState.branches }

                ClosedParen ->
                    case parseState.branches of
                        -- done
                        expr :: [] ->
                            Just { parseState | branches = [], done = Just expr }

                        -- not done
                        (Branch xs) :: (Branch ys) :: rest ->
                            Just { parseState | branches = (Branch ((Branch xs) :: ys)) :: rest }

                        _ ->
                            Nothing

                NotAParen ->
                    case parseState.branches of
                        (Branch x) :: rest ->
                            Just { parseState | branches = (Branch ((Leaf token) :: x)) :: rest }

                        [] ->
                            Just { parseState | branches = [ Leaf token ], done = Just (Leaf token) }

                        _ ->
                            Nothing



-- HELPERS


elmExampleString : String
elmExampleString =
    """
  [Dict.fromList [("channels",[]),("chordText",[]),("guitarVoicings",[]),("ladderHtml",[(CharT,["triad","dots"]),(CharS,["guitarSustain","pianoSustain","noSustain"])]),("pianoRoll",[])]]
  """


example : Maybe (Tree String)
example =
    elmExampleString |> tokenizeElmPrint |> parseTokens elmPrintConfig


doElm : String -> Maybe (Tree String)
doElm =
    tokenizeElmPrint >> parseTokens elmPrintConfig


padSymbols : List Char -> String -> String
padSymbols symbols input =
    let
        f c s =
            if List.member c symbols then
                String.concat [ s, " ", String.fromChar c, " " ]
            else
                String.append s (String.fromChar c)
    in
        input
            |> String.toList
            |> List.foldl f ""


tokenizeElmPrint : String -> List String
tokenizeElmPrint input =
    input
        |> stringReplace "," " "
        |> padSymbols (String.toList "()[]{}")
        |> String.split " "
        |> List.map String.trim
        |> List.filter ((/=) "")


{-| Parse a string of scheme code. Special forms are stored as symbols. Could
pattern match special forms here and produce syntax errors, rather than
evaluation errors in the case structure of eval.
-}
parseScheme : String -> Maybe (Tree String)
parseScheme input =
    input
        |> padSymbols [ '(', ')' ]
        |> String.split " "
        |> List.map String.trim
        |> List.filter ((/=) "")
        |> parseTokens schemeConfig


emptyParse : ParseState
emptyParse =
    { branches = [], done = Nothing }


parseTokens : ParseConfig -> List String -> Maybe (Tree String)
parseTokens config tokens =
    foldm (parseToken config) (Just emptyParse) tokens
        |> Maybe.andThen .done
        |> Maybe.map reverseTree


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f tree =
    case tree of
        Leaf x ->
            Leaf (f x)

        Branch xs ->
            Branch (List.map (mapTree f) xs)


filterTree : (a -> Bool) -> Tree a -> Tree a
filterTree f tree =
    let
        g n =
            case n of
                Leaf x ->
                    f x

                Branch _ ->
                    True
    in
        case tree of
            Leaf x ->
                Leaf x

            Branch xs ->
                List.filter g xs
                    |> List.map (filterTree f)
                    |> Branch


traverseTree : (a -> Maybe b) -> Tree a -> Maybe (Tree b)
traverseTree f tree =
    case tree of
        Leaf x ->
            f x |> Maybe.map Leaf

        Branch xs ->
            (Maybe.Extra.traverse (traverseTree f) xs)
                |> Maybe.map Branch


reverseTree : Tree a -> Tree a
reverseTree tree =
    case tree of
        Leaf _ ->
            tree

        Branch xs ->
            Branch (xs |> List.map reverseTree |> List.reverse)


foldm : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
foldm f acc xs =
    case xs of
        [] ->
            acc

        x :: rest ->
            foldm f (Maybe.andThen (f x) acc) rest


{-| replace String.Extra.replace since String.Extra 1.4.0 is broken
-}
stringReplace : String -> String -> String -> String
stringReplace pattern replacement source =
    source
        |> String.split pattern
        |> List.intersperse replacement
        |> String.join ""
