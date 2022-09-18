module DataUtilities exposing (..)

import DataParser exposing (Area, ClimbingRoute, ClimbingRouteKind, Sector)
import Utilities


type alias Match a comparable =
    comparable -> a -> Bool


type alias Filter a comparable =
    comparable -> List a -> List a



-- | Generic


matchByName : Match { a | name : String } String
matchByName filter item =
    String.contains (String.toLower filter) (String.toLower item.name)



--| Area


sortAreas : List Area -> List Area
sortAreas =
    List.sortBy .name


matchAreaByName : Match Area String
matchAreaByName =
    matchByName


filterAreasByName : Filter Area String
filterAreasByName filter areas =
    List.filter (matchAreaByName filter) areas



--| Sector


matchSectorByName : Match Sector String
matchSectorByName =
    matchByName


filterSectorsByName : Filter Sector String
filterSectorsByName filter sectors =
    List.filter (matchSectorByName filter) sectors


matchSectorByAreaId : Match Sector Int
matchSectorByAreaId id sector =
    id == sector.areaId


filterSectorsByAreaId : Filter Sector Int
filterSectorsByAreaId filter sectors =
    List.filter (matchSectorByAreaId filter) sectors



--| ClimbingRoute


sortRoutes : List ClimbingRoute -> List ClimbingRoute
sortRoutes =
    Utilities.sortByDescending .grade


filterRoutes : String -> List Sector -> Maybe ClimbingRouteKind -> List ClimbingRoute -> List ClimbingRoute
filterRoutes routeFilter selectedSectors kind routes =
    (filterRoutesByName routeFilter
        >> filterRoutesBySectors selectedSectors
        >> filterRoutesByKind kind
    )
        routes


filterRoutesByName : String -> List ClimbingRoute -> List ClimbingRoute
filterRoutesByName filter =
    List.filter (\route -> String.contains (String.toLower filter) (String.toLower route.name))


matchRouteBySectors : List Sector -> ClimbingRoute -> Bool
matchRouteBySectors sectors route =
    List.isEmpty sectors || List.member route.sectorId (List.map .id sectors)


filterRoutesBySectors : List Sector -> List ClimbingRoute -> List ClimbingRoute
filterRoutesBySectors sectors =
    List.filter (matchRouteBySectors sectors)


matchRouteByKind : Maybe ClimbingRouteKind -> ClimbingRoute -> Bool
matchRouteByKind kind route =
    Maybe.map (\k -> route.kind == k) kind |> Maybe.withDefault True


filterRoutesByKind : Maybe ClimbingRouteKind -> List ClimbingRoute -> List ClimbingRoute
filterRoutesByKind kind =
    List.filter (matchRouteByKind kind)
