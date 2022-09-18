module ModelAccessorsTest exposing (testRemoval)

import DataParser exposing (Area, Ascent, AscentKind(..), ClimbingRoute, Sector)
import Date exposing (Date)
import Dict exposing (Dict)
import Expect
import Init
import Model exposing (Model)
import ModelAccessors as MA
import Test exposing (Test, describe, test)
import Utilities



--| Values


testRemoval : Test
testRemoval =
    let
        ( model, _ ) =
            Init.init
                { storageCache = ""
                , posixTime = 0
                }

        { areas, sectors, climbingRoutes, ascents } =
            data

        updatedModel : Model
        updatedModel =
            MA.deleteRoute 1 { model | climbingRoutes = climbingRoutes, ascents = ascents }

        routeIds =
            mapToId updatedModel.climbingRoutes

        ascentIds =
            mapToId updatedModel.ascents
    in
    describe "remove route 1"
        [ test "ascents should be removed" <|
            \_ ->
                Expect.equalLists ascentIds (Dict.filter (\_ a -> not <| a.routeId == 1) ascents |> mapToId)
        , test "route should be removed" <|
            \_ ->
                Expect.equalLists routeIds (Dict.filter (\_ c -> not <| c.id == 1) climbingRoutes |> mapToId)
        ]



--| MockData


data : { areas : Dict Int Area, sectors : Dict Int Sector, climbingRoutes : Dict Int ClimbingRoute, ascents : Dict Int Ascent }
data =
    { areas =
        Dict.empty
            |> addArea 1
            |> addArea 2
            |> addArea 3
    , sectors =
        Dict.empty
            |> addSector 1 1
            |> addSector 2 1
            |> addSector 3 2
            |> addSector 4 1
            |> addSector 5 2
    , climbingRoutes =
        Dict.empty
            |> addRoute 1 1
            |> addRoute 2 1
            |> addRoute 3 5
            |> addRoute 4 3
            |> addRoute 5 2
    , ascents =
        Dict.empty
            |> addAscent 1 1
            |> addAscent 2 1
            |> addAscent 3 2
            |> addAscent 4 1
            |> addAscent 5 2
    }



--| Utilties


addArea : Int -> Dict Int Area -> Dict Int Area
addArea id =
    Dict.insert id
        { id = id
        , name = ""
        , country = ""
        }


addSector : Int -> Int -> Dict Int Sector -> Dict Int Sector
addSector id areaId =
    Dict.insert id
        { id = id
        , areaId = areaId
        , name = ""
        }


addRoute : Int -> Int -> Dict Int ClimbingRoute -> Dict Int ClimbingRoute
addRoute id sectorId =
    Dict.insert id
        { id = id
        , sectorId = sectorId
        , name = ""
        , grade = ""
        , comment = Nothing
        , kind = Data.Boulder
        , media = []
        }


addAscent : Int -> Int -> Dict Int Ascent -> Dict Int Ascent
addAscent id routeId =
    Dict.insert id
        { id = id
        , routeId = routeId
        , date = aDate
        , comment = Nothing
        , kind = Onsight
        }


aDate : Date
aDate =
    Date.fromOrdinalDate 2018 269


mapToId : Dict comparable { a | id : b } -> List b
mapToId =
    Utilities.dictToList >> List.map .id
