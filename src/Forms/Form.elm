module Forms.Form exposing (Form(..), append, extract, map, mapAndReturn, mapErrors, succeed)

import Result.Extra as RExtra


type alias Test =
    { a : String
    , b : Int
    }


type Form values result
    = Idle values
    | Valid result values
    | Invalid (List String) values


map : (values -> values) -> Form values result -> Form values result
map f form =
    case form of
        Idle values ->
            Idle (f values)

        Valid result values ->
            Valid result (f values)

        Invalid s values ->
            Invalid s (f values)


mapResult : (result -> newResult) -> Form values result -> Form values newResult
mapResult f form =
    case form of
        Idle values ->
            Idle values

        Valid result values ->
            Valid (f result) values

        Invalid s values ->
            Invalid s values


mapErrors : (String -> a) -> Form values result -> List a
mapErrors f form =
    List.map f (errors form)


mapAndReturn : (values -> ( values, x )) -> Form values result -> ( Form values result, x )
mapAndReturn f form =
    let
        ( newValues, newX ) =
            f <| get form

        newForm =
            map (\_ -> newValues) form
    in
    ( newForm, newX )


extract : (a -> b) -> Form a r -> b
extract extractor =
    extractor << get


succeed : r -> Form a x -> Form a r
succeed r f =
    Valid r (get f)


append : (a -> Result String b) -> Form a (b -> c) -> Form a c
append f form =
    let
        result =
            f <| get form
    in
    case result of
        Ok v ->
            mapResult (\x -> x v) form

        Err x ->
            Invalid (x :: errors form) (get form)



--| Local


errors : Form a r -> List String
errors f =
    case f of
        Invalid x _ ->
            x

        _ ->
            []


get : Form a r -> a
get form =
    case form of
        Idle value ->
            value

        Valid _ values ->
            values

        Invalid _ values ->
            values
