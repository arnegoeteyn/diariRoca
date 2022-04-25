module Form exposing (..)

import Data exposing (Ascent, AscentKind(..), ClimbingRoute, ClimbingRouteKind(..))
import Dict
import Model exposing (Model)



--| Generic


newId : Dict.Dict Int a -> Int
newId dict =
    (Maybe.withDefault 1 <| List.head <| List.sortBy (\x -> x * -1) (Dict.keys dict)) + 1


climbingRouteFromForm : Model -> ClimbingRoute
climbingRouteFromForm model =
    let
        form =
            model.climbingRouteForm

        id =
            Maybe.withDefault (newId model.climbingRoutes) model.climbingRouteForm.id

        sectorId =
            List.head form.selected |> Maybe.map .id |> Maybe.withDefault 1
    in
    { name = Maybe.withDefault "" form.name
    , grade = Maybe.withDefault "" form.grade
    , comment = form.comment
    , media = []
    , sectorId = sectorId
    , id = id
    , kind = Maybe.withDefault Sport form.kind
    }


ascentFromForm : Model -> Ascent
ascentFromForm model =
    let
        form =
            model.ascentForm

        id =
            Maybe.withDefault (newId model.ascents) model.ascentForm.id

        routeId =
            Maybe.withDefault -1 (Maybe.map .id model.selectedClimbingRoute)
    in
    { comment = form.comment
    , routeId = routeId
    , id = id
    , kind = Maybe.withDefault Redpoint form.kind
    , date = Maybe.withDefault model.startUpDate form.date
    }



--| Utilities


updateName : { a | name : b } -> b -> { a | name : b }
updateName form value =
    { form | name = value }


updateGrade : { a | grade : b } -> b -> { a | grade : b }
updateGrade form value =
    { form | grade = value }


updateComment : { a | comment : b } -> b -> { a | comment : b }
updateComment form value =
    { form | comment = value }


updateKind : { a | kind : b } -> b -> { a | kind : b }
updateKind form value =
    { form | kind = value }
