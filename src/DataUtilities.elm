module DataUtilities exposing (..)

import Data exposing (ClimbingRoute, Sector)
import Utilities


sortRoutes : List ClimbingRoute -> List ClimbingRoute
sortRoutes =
    Utilities.sortByDescending .grade


filterRoutes : String -> List Sector -> List ClimbingRoute -> List ClimbingRoute
filterRoutes routeFilter selectedSectors routes =
    let
        filter =
            filterRoutesByName routeFilter >> filterRoutesBySectors selectedSectors
    in
    filter routes


filterRoutesByName : String -> List ClimbingRoute -> List ClimbingRoute
filterRoutesByName filter =
    List.filter (\route -> String.contains (String.toLower filter) (String.toLower route.name))


matchRouteBySectors : List Sector -> ClimbingRoute -> Bool
matchRouteBySectors sectors route =
    List.isEmpty sectors || List.member route.sectorId (List.map .id sectors)


filterRoutesBySectors : List Sector -> List ClimbingRoute -> List ClimbingRoute
filterRoutesBySectors sectors =
    List.filter (matchRouteBySectors sectors)


matchSectorByName : String -> Sector -> Bool
matchSectorByName filter sector =
    String.contains (String.toLower filter) (String.toLower sector.name)


filterSectorsByName : String -> List Sector -> List Sector
filterSectorsByName filter sectors =
    List.filter (matchSectorByName filter) sectors
