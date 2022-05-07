module Utilities exposing (..)

import Dict
import Set exposing (Set)



--| Maybe


catMaybe : List (Maybe a) -> List a
catMaybe l =
    List.filterMap identity l


catMaybeSet : Set (Maybe comparable) -> Set comparable
catMaybeSet =
    Set.toList >> catMaybe >> Set.fromList



--| String


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



--| List


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


filterList : List ( a, Bool ) -> List a
filterList =
    filterAndReplaceList << List.map (\( x, y ) -> ( x, y, Nothing ))


filterAndReplaceList : List ( a, Bool, Maybe a ) -> List a
filterAndReplaceList list =
    case list of
        [] ->
            []

        ( style, b, maybeAlternative ) :: xs ->
            if b then
                style :: filterAndReplaceList xs

            else
                case maybeAlternative of
                    Nothing ->
                        filterAndReplaceList xs

                    Just alternative ->
                        alternative :: filterAndReplaceList xs


removeFirst : (a -> Bool) -> List a -> List a
removeFirst f l =
    List.foldr
        (\item ( acc, matched ) ->
            if matched then
                ( item :: acc, True )

            else if f item then
                ( item :: acc, False )

            else
                ( acc, True )
        )
        ( [], False )
        l
        |> Tuple.first



--| Misc


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
