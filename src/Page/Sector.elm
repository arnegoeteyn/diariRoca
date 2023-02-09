module Page.Sector exposing (..)

import Data exposing (Area, Data, Sector)
import DataAccessors as DA
import DataUtilities as DU
import Dict
import Form.Criterium exposing (formSelectionWithSearchCriterium, formTextCriterium, updateSelectCriteriumMsg)
import Form.Form as Form
import Form.Forms exposing (SelectionCriterium, idForForm, updateName, validateNonEmpty, viewErrors)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Modal
import Select
import Session
import Skeleton
import Tailwind.Utilities as TW
import Utilities exposing (orElse)
import View.Button as Button



-- Model


type alias ModelContent =
    { sectorId : Int
    }


type alias Model =
    Session.ModelEncapsulated ModelContent



-- Init


init : Session.Model -> Int -> ( Model, Cmd Msg )
init session sectorId =
    ( { session = session
      , sectorId = sectorId
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



--View


view : Model -> Skeleton.Details Msg
view model =
    { title = "Sectors"
    , warning = Skeleton.NoProblems
    , session = model.session
    , kids =
        [ H.p [] [ H.text "hallowkes" ]
        ]
    }



-- Utilities
