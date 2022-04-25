module ModelAccessors exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector)
import Dict exposing (Dict)
import Form
import Model exposing (Model)
import Set
import Utilities exposing (catMaybe)



--| Area


getArea : Model -> Int -> Maybe Area
getArea m i =
    Dict.get i m.areas



--| Sector


getSector : Model -> Int -> Maybe Sector
getSector m i =
    Dict.get i m.sectors



--| ClimbingRoute


getClimbingRoute : Model -> Int -> Maybe ClimbingRoute
getClimbingRoute m i =
    Dict.get i m.climbingRoutes


addRouteFromForm : Model -> Dict Int ClimbingRoute
addRouteFromForm model =
    let
        newRoute =
            Form.climbingRouteFromForm model
    in
    Dict.insert newRoute.id newRoute model.climbingRoutes


deleteRoute : Model -> Int -> Model
deleteRoute model i =
    case Dict.get i model.climbingRoutes of
        Nothing ->
            model

        Just c ->
            let
                ascentsModel : Model
                ascentsModel =
                    Dict.foldr
                        (\_ value accModel ->
                            if isAscentOf c value then
                                deleteAscent accModel value.id

                            else
                                accModel
                        )
                        model
                        model.ascents
            in
            { ascentsModel | climbingRoutes = Dict.remove i model.climbingRoutes }


isAscentOf : ClimbingRoute -> Ascent -> Bool
isAscentOf c a =
    c.id == a.routeId


getAscents : Model -> ClimbingRoute -> List Ascent
getAscents m c =
    Utilities.dictToList m.ascents |> List.filter (isAscentOf c)



--| Ascent


getAscent : Model -> Int -> Maybe Ascent
getAscent m i =
    Dict.get i m.ascents


deleteAscent : Model -> Int -> Model
deleteAscent m i =
    { m | ascents = Dict.remove i m.ascents }



-- deleteAscent : Model -> Int -> Dict Int Ascent
-- deleteAscent m i =
--     Dict.remove i m.ascents
--| Trip
