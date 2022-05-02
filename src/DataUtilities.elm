module DataUtilities exposing (..)

import Data exposing (ClimbingRoute, ClimbingRouteKind, Sector)
import Utilities


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


matchSectorByName : String -> Sector -> Bool
matchSectorByName filter sector =
    String.contains (String.toLower filter) (String.toLower sector.name)


filterSectorsByName : String -> List Sector -> List Sector
filterSectorsByName filter sectors =
    List.filter (matchSectorByName filter) sectors
