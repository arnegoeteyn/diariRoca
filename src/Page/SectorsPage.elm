module Page.SectorsPage exposing (..)

import Data exposing (Area, Sector)
import DataUtilities as DU
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg(..), SectorsPageMsg(..))
import Model exposing (Model)
import Tailwind.Utilities as TW
import Utilities


view : Model -> H.Html Msg
view model =
    H.div [ A.css [ TW.grid, TW.grid_cols_2 ] ]
        [ viewAreas { selectedArea = Nothing } (Utilities.dictToList model.areas |> DU.sortAreas)
        , viewSectors { selectedSector = Nothing } (filterSectors model)
        ]


viewAreas : { selectedArea : Maybe Area } -> List Area -> H.Html Msg
viewAreas options =
    H.div []
        << List.map
            (\area ->
                H.div
                    [ A.id <| "area-" ++ String.fromInt area.id
                    , A.css [ TW.border, TW.border_solid, TW.py_4 ]
                    , E.onClick ((SectorsPageMessage << AreaSelected) (Just area))
                    ]
                    [ viewArea { area = area, selected = False } ]
            )


viewArea : { area : Area, selected : Bool } -> Html Msg
viewArea { area, selected } =
    H.div
        [ A.css [ TW.flex ]
        ]
        [ H.text <| area.name
        ]


viewSectors : { selectedSector : Maybe Sector } -> List Sector -> H.Html Msg
viewSectors options =
    H.div []
        << List.map
            (\sector ->
                H.div
                    [ A.id <| "sector-" ++ String.fromInt sector.id
                    , A.css [ TW.border, TW.border_solid, TW.py_4 ]
                    , E.onClick Message.Dummy
                    ]
                    [ viewSectorRow { sector = sector, selected = False } ]
            )


viewSectorRow : { sector : Sector, selected : Bool } -> Html Msg
viewSectorRow { sector, selected } =
    H.div
        [ A.css [ TW.flex ]
        ]
        [ H.div [ A.css [] ] [ H.text <| sector.name ]
        ]



-- Utilities


filterSectors : Model -> List Sector
filterSectors model =
    case model.sectorsPageModel.selectedArea of
        Nothing ->
            Utilities.dictToList model.sectors

        Just area ->
            DU.filterSectorsByAreaId area.id (Utilities.dictToList model.sectors)
