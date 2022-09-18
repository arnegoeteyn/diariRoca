module General exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)



-- Model


type alias Model =
    { data : Data
    }


type alias Data =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip
    }


type alias ClimbingRoute =
    { id : Int
    , sectorId : Int
    , name : String
    , grade : String
    , comment : Maybe String
    , beta : Maybe String
    , kind : ClimbingRouteKind
    , media : List Media
    }


type ClimbingRouteKind
    = Boulder
    | Sport


type alias Media =
    { link : String, label : String }


type alias Ascent =
    { id : Int
    , routeId : Int
    , date : Date
    , comment : Maybe String
    , kind : AscentKind
    }


type AscentKind
    = Onsight
    | Flash
    | SecondGo
    | Redpoint
    | Repeat


type alias Sector =
    { id : Int
    , areaId : Int
    , name : String
    }


type alias Area =
    { id : Int
    , name : String
    , country : String
    }


type alias Trip =
    { id : Int
    , from : Date
    , to : Date
    }



-- Update


type Msg
    = None
    | DeleteClimbingRouteConfirmation ClimbingRoute
    | DeleteClimbingRouteRequested ClimbingRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        DeleteClimbingRouteRequested route ->
            -- ( { model | modal = Model.DeleteClimbingRouteRequestModal route }, Cmd.none )
            ( model, Cmd.none )

        DeleteClimbingRouteConfirmation route ->
            ( model, Cmd.none )



-- Utilities


ignore : ( a, b, c ) -> ( a, b )
ignore ( a, b, c ) =
    ( a, b )


withNothing : ( a, b ) -> ( a, b, Msg )
withNothing ( a, b ) =
    ( a, b, None )
