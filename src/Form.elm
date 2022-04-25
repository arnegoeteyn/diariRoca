module Form exposing (..)

import Data exposing (ClimbingRoute, ClimbingRouteKind(..))
import Dict
import Model exposing (Model)
import Set



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
    , kind = Sport
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
