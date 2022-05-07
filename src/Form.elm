module Form exposing (..)

import Data exposing (Ascent, AscentKind(..), ClimbingRoute, ClimbingRouteKind(..), Media)
import Dict
import Model exposing (Model)



--| Generic


newId : Dict.Dict Int a -> Int
newId dict =
    (Maybe.withDefault 1 <| List.head <| List.sortBy (\x -> x * -1) (Dict.keys dict)) + 1



--| ClimbingRoute


climbingRouteFromForm : Model -> ClimbingRoute
climbingRouteFromForm model =
    let
        form =
            model.climbingRoutesPageModel.climbingRouteForm

        id =
            Maybe.withDefault (newId model.climbingRoutes) model.climbingRoutesPageModel.climbingRouteForm.id

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


mediaFromForm : Model -> Maybe Media
mediaFromForm model =
    Maybe.map2 Media model.climbingRoutesPageModel.mediaLink model.climbingRoutesPageModel.mediaLabel



--| Ascent


ascentFromForm : Model -> Ascent
ascentFromForm model =
    let
        crpModel =
            model.climbingRoutesPageModel

        form =
            crpModel.ascentForm

        id =
            Maybe.withDefault (newId model.ascents) crpModel.ascentForm.id

        routeId =
            Maybe.withDefault -1 (Maybe.map .id crpModel.selectedClimbingRoute)
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
