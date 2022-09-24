module DataAccessors exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Data, Sector, Trip)
import Date exposing (Date)
import Dict exposing (Dict)
import Model exposing (Model)
import Set
import Utilities



--| Area


getArea : Data -> Int -> Maybe Area
getArea d i =
    Dict.get i d.areas


getAreaName : Data -> Int -> Maybe String
getAreaName d i =
    getArea d i |> Maybe.map .name


getAreaNameSafe : Data -> Int -> String
getAreaNameSafe d i =
    getArea d i |> Maybe.map .name |> Maybe.withDefault "N/A"


isSectorOf : Area -> Sector -> Bool
isSectorOf a s =
    a.id == s.areaId


deleteArea : Int -> Data -> Data
deleteArea i m =
    case Dict.get i m.areas of
        Nothing ->
            m

        Just a ->
            let
                sectorsModel : Data
                sectorsModel =
                    Dict.foldr
                        (\_ value accModel ->
                            if isSectorOf a value then
                                deleteSector value.id accModel

                            else
                                accModel
                        )
                        m
                        m.sectors
            in
            { sectorsModel | areas = Dict.remove i m.areas }



--| Sector


getSector : Data -> Int -> Maybe Sector
getSector d i =
    Dict.get i d.sectors


getSectorName : Data -> Int -> Maybe String
getSectorName d i =
    getSector d i |> Maybe.map .name


getSectorAndAreaNameSafe : Data -> Int -> String
getSectorAndAreaNameSafe d i =
    case getSector d i of
        Nothing ->
            "N/A"

        Just sector ->
            Utilities.stringFromList [ sector.name, " [", getAreaNameSafe d sector.areaId, "]" ]


getSectorNameSafe : Data -> Int -> String
getSectorNameSafe d i =
    getSector d i |> Maybe.map .name |> Maybe.withDefault "N/A"


isClimbingRouteOf : Sector -> ClimbingRoute -> Bool
isClimbingRouteOf s c =
    s.id == c.sectorId


deleteSector : Int -> Data -> Data
deleteSector i m =
    case Dict.get i m.sectors of
        Nothing ->
            m

        Just s ->
            let
                climbingRoutesModel : Data
                climbingRoutesModel =
                    Dict.foldr
                        (\_ value accModel ->
                            if isClimbingRouteOf s value then
                                deleteRoute value.id accModel

                            else
                                accModel
                        )
                        m
                        m.climbingRoutes
            in
            { climbingRoutesModel | sectors = Dict.remove i m.sectors }



--| ClimbingRoute


addClimbingRoute : ClimbingRoute -> Data -> Data
addClimbingRoute climbingRoute data =
    { data | climbingRoutes = Dict.insert climbingRoute.id climbingRoute data.climbingRoutes }


getClimbingRoute : Data -> Int -> Maybe ClimbingRoute
getClimbingRoute m i =
    Dict.get i m.climbingRoutes


deleteRoute : Int -> Data -> Data
deleteRoute i model =
    case Dict.get i model.climbingRoutes of
        Nothing ->
            model

        Just c ->
            let
                ascentsModel : Data
                ascentsModel =
                    Dict.foldr
                        (\_ value accModel ->
                            if isAscentOf c value then
                                deleteAscent value.id accModel

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


getAscents : Data -> ClimbingRoute -> List Ascent
getAscents d c =
    Utilities.dictToList d.ascents |> List.filter (isAscentOf c) |> Utilities.sortByDescending (.date >> Date.toIsoString)



--| Ascent


addAscent : Ascent -> Data -> Data
addAscent ascent data =
    { data | ascents = Dict.insert ascent.id ascent data.ascents }


getAscent : Model -> Int -> Maybe Ascent
getAscent m i =
    Dict.get i m.ascents


getClimbingRouteFromAscent : Data -> Ascent -> Maybe ClimbingRoute
getClimbingRouteFromAscent data c =
    getClimbingRoute data c.routeId


deleteAscent : Int -> Data -> Data
deleteAscent i m =
    { m | ascents = Dict.remove i m.ascents }



--| Trip


getTripFromDate : Model -> Date -> Maybe Trip
getTripFromDate m date =
    Dict.filter (\_ t -> Date.isBetween t.from t.to date) m.trips |> Dict.values |> List.head


getRoutesFromTrip : Model -> Trip -> Dict String Int
getRoutesFromTrip model trip =
    Utilities.filterDictValue (.date >> Date.isBetween trip.from trip.to) model.ascents
        |> Dict.map (\_ v -> v.routeId)
        |> Dict.values
        |> Set.fromList
        |> Set.toList
        |> List.filterMap (\v -> Dict.get v model.climbingRoutes)
        |> List.foldl (\route dict -> Dict.update route.grade (\x -> Just <| Maybe.withDefault 0 x + 1) dict) Dict.empty
