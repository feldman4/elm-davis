module Audio.Component
    exposing
        ( viewComponents
        , selected
        , copySelection
        , toggleSelectionNumKey
        , updateAllConfig
        , Component
        , Config
        , Selection
        )

import Array exposing (Array)
import Array.Extra
import Audio.Utility exposing (rotate)
import Dict exposing (Dict)
import Html exposing (Html)
import Keyboard.Extra


main : Html msg
main =
    Html.text ""


type alias Selection a =
    List ( Bool, a )


{-| Cheapo namespace for configurations. Each component is a key. Configuration
values are enums. Enums rotate when the corresponding key is pressed.

Could make this parametric if needed.
-}
type alias Config a =
    Dict String (List ( a, List String ))


type alias Component a =
    List String -> Html a


viewComponents : Config a -> List (Component msg) -> List (Html msg)
viewComponents config components =
    components |> List.map (\f -> f (flattenConfig config))


{-| convenience
-}
viewComponentModel :
    { b | components : List (Component msg), config : Config a }
    -> List (Html msg)
viewComponentModel { components, config } =
    viewComponents config components


flattenConfig : Config a -> List String
flattenConfig config =
    let
        f ( name, xs ) =
            xs
                |> List.filterMap g
                |> List.map ((++) (name ++ "."))

        g ( key, options ) =
            options |> List.head
    in
        config |> Dict.toList |> List.concatMap f


updateNamedConfig : String -> b -> Config b -> Config b
updateNamedConfig name key config =
    config |> Dict.update name (Maybe.map (List.map (updateEntry key)))


updateAllConfig : a -> Config a -> Config a
updateAllConfig key config =
    config |> Dict.map (\a b -> b |> List.map (updateEntry key))


updateEntry : b -> ( b, List a ) -> ( b, List a )
updateEntry key ( key_, items ) =
    if key == key_ then
        ( key_, rotate 1 items )
    else
        ( key_, items )


selected : Selection a -> List a
selected xs =
    xs
        |> List.filter (\( a, b ) -> a)
        |> List.map (\( a, b ) -> b)


copySelection : Selection a -> Selection b -> Selection b
copySelection =
    List.map2 (\( bool1, a ) ( bool2, b ) -> ( bool1, b ))


keyToInt : Keyboard.Extra.Key -> Maybe Int
keyToInt key =
    let
        num =
            (key |> Keyboard.Extra.toCode) - 48
    in
        if (0 <= num) && (num <= 9) then
            Just num
        else
            Nothing


{-| Assumes you mean the first item when you press "1".
-}
toggleSelectionNumKey : Keyboard.Extra.Key -> List ( Bool, a ) -> List ( Bool, a )
toggleSelectionNumKey key =
    toggleSelection (keyToInt key |> Maybe.map (\x -> x - 1) |> Maybe.withDefault -1)


toggleSelection : Int -> List ( Bool, a ) -> List ( Bool, a )
toggleSelection i xs =
    let
        f ( a, b ) =
            ( not a, b )
    in
        Array.Extra.update i f (Array.fromList xs)
            |> Array.toList
