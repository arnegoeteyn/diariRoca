module Tests exposing (testRemoval)

import Data exposing (Ascent, AscentKind(..), ClimbingRoute)
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

        ( routes, ascents ) =
            data

        updatedModel : Model
        updatedModel =
            MA.deleteRoute { model | climbingRoutes = routes, ascents = ascents } 1

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
                Expect.equalLists routeIds (Dict.filter (\_ c -> not <| c.id == 1) routes |> mapToId)
        ]



--| MockData


data : ( Dict Int ClimbingRoute, Dict Int Ascent )
data =
    ( Dict.empty
        |> addRoute 1 1
        |> addRoute 2 1
        |> addRoute 3 1
    , Dict.empty
        |> addAscent 1 1
        |> addAscent 2 1
        |> addAscent 3 2
        |> addAscent 4 1
        |> addAscent 5 2
    )



--| Utilties


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
