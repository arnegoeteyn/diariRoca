module ModelAccessors exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Data, Sector, Trip)
import Date exposing (Date)
import Dict exposing (Dict)
import Model exposing (Model)
import Set
import Utilities



--| Area


getArea : Model -> Int -> Maybe Area
getArea m i =
    Dict.get i m.areas


getAreaName : Model -> Int -> Maybe String
getAreaName m i =
    getArea m i |> Maybe.map .name


getAreaNameSafe : Model -> Int -> String
getAreaNameSafe m i =
    getArea m i |> Maybe.map .name |> Maybe.withDefault "N/A"


isSectorOf : Area -> Sector -> Bool
isSectorOf a s =
    a.id == s.areaId


deleteArea : Int -> Model -> Model
deleteArea i m =
    case Dict.get i m.areas of
        Nothing ->
            m

        Just a ->
            let
                sectorsModel : Model
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


getSector : Model -> Int -> Maybe Sector
getSector m i =
    Dict.get i m.sectors


getSectorName : Model -> Int -> Maybe String
getSectorName m i =
    getSector m i |> Maybe.map .name


getSectorAndAreaNameSafe : Model -> Int -> String
getSectorAndAreaNameSafe m i =
    case getSector m i of
        Nothing ->
            "N/A"

        Just sector ->
            Utilities.stringFromList [ sector.name, " [", getAreaNameSafe m sector.areaId, "]" ]


getSectorNameSafe : Model -> Int -> String
getSectorNameSafe m i =
    getSector m i |> Maybe.map .name |> Maybe.withDefault "N/A"


isClimbingRouteOf : Sector -> ClimbingRoute -> Bool
isClimbingRouteOf s c =
    s.id == c.sectorId


deleteSector : Int -> Model -> Model
deleteSector i m =
    case Dict.get i m.sectors of
        Nothing ->
            m

        Just s ->
            let
                climbingRoutesModel : Model
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


getClimbingRoute : Data -> Int -> Maybe ClimbingRoute
getClimbingRoute m i =
    Dict.get i m.climbingRoutes


deleteRoute : Int -> Model -> Model
deleteRoute i model =
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


getAscents : Model -> ClimbingRoute -> List Ascent
getAscents m c =
    Utilities.dictToList m.ascents |> List.filter (isAscentOf c) |> Utilities.sortByDescending (.date >> Date.toIsoString)



--| Ascent


getAscent : Model -> Int -> Maybe Ascent
getAscent m i =
    Dict.get i m.ascents


getClimbingRouteFromAscent : Data -> Ascent -> Maybe ClimbingRoute
getClimbingRouteFromAscent data c =
    getClimbingRoute data c.routeId


deleteAscent : Int -> Model -> Model
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
