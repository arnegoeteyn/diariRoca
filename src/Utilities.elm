module Utilities exposing (..)

import Dict
import Set exposing (Set)


catMaybe : List (Maybe a) -> List a
catMaybe l =
    List.filterMap identity l


catMaybeSet : Set (Maybe comparable) -> Set comparable
catMaybeSet =
    Set.toList >> catMaybe >> Set.fromList


stringFromList : List String -> String
stringFromList =
    stringFromListWith ""


stringFromListWith : String -> List String -> String
stringFromListWith seperator list =
    case list of
        [] ->
            ""

        [ x ] ->
            x

        x :: xs ->
            x ++ seperator ++ stringFromListWith seperator xs


listToMaybe : List a -> Maybe (List a)
listToMaybe l =
    if List.isEmpty l then
        Nothing

    else
        Just l


sortByDescending : (a -> comparable) -> List a -> List a
sortByDescending func =
    List.sortWith
        (\a b ->
            case compare (func a) (func b) of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT
        )


sortDescending : List comparable -> List comparable
sortDescending =
    sortByDescending identity


addIfNotPresent : a -> List a -> List a
addIfNotPresent item list =
    if List.member item list then
        list

    else
        item :: list


dictToList : Dict.Dict comparable a -> List a
dictToList =
    Dict.toList >> List.map Tuple.second
