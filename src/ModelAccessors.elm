module ModelAccessors exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector, Trip)
import Date exposing (Date)
import Dict exposing (Dict)
import Forms.Forms
import Model exposing (Model)
import Utilities



--| Area


getArea : Model -> Int -> Maybe Area
getArea m i =
    Dict.get i m.areas



--| Sector


getSector : Model -> Int -> Maybe Sector
getSector m i =
    Dict.get i m.sectors


getSectorName : Model -> Int -> String
getSectorName m i =
    getSector m i |> Maybe.map .name |> Maybe.withDefault "N/A"



--| ClimbingRoute


getClimbingRoute : Model -> Int -> Maybe ClimbingRoute
getClimbingRoute m i =
    Dict.get i m.climbingRoutes


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
    Utilities.dictToList m.ascents |> List.filter (isAscentOf c) |> Utilities.sortByDescending (.date >> Date.toIsoString)



--| Ascent


getAscent : Model -> Int -> Maybe Ascent
getAscent m i =
    Dict.get i m.ascents


getClimbingRouteFromAscent : Model -> Ascent -> Maybe ClimbingRoute
getClimbingRouteFromAscent m c =
    getClimbingRoute m c.routeId


deleteAscent : Model -> Int -> Model
deleteAscent m i =
    { m | ascents = Dict.remove i m.ascents }



--| Trip


getTripFromDate : Model -> Date -> Maybe Trip
getTripFromDate m date =
    Dict.filter (\_ t -> Date.isBetween t.from t.to date) m.trips |> Dict.values |> List.head
