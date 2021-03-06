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
import View.Button as Button


view : Model -> H.Html Msg
view model =
    H.div [ A.css [ TW.grid, TW.grid_cols_2 ] ]
        [ viewAreas { selectedArea = Nothing } (Utilities.dictToList model.areas |> DU.sortAreas)
        , viewSectors { selectedSector = Nothing } (filterSectors model)
        ]


viewAreas : { selectedArea : Maybe Area } -> List Area -> H.Html Msg
viewAreas options areas =
    H.div
        []
        [ H.text "Areas"
        , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenAreaForm Nothing))
        , H.div []
            (List.map
                (\area ->
                    H.div
                        [ A.id <| "area-" ++ String.fromInt area.id
                        , A.css [ TW.border, TW.border_solid, TW.py_4 ]
                        , E.onClick ((SectorsPageMessage << AreaSelected) (Just area))
                        ]
                        [ viewArea { area = area, selected = False } ]
                )
                areas
            )
        ]


viewArea : { area : Area, selected : Bool } -> Html Msg
viewArea { area, selected } =
    H.div
        [ A.css [ TW.flex ]
        ]
        [ H.text <| area.name
        , H.button [ E.onClick <| OpenAreaForm (Just area) ] [ H.text "edit" ]
        , H.button [ E.onClick <| DeleteAreaRequested area ] [ H.text "delete" ]
        ]


viewSectors : { selectedSector : Maybe Sector } -> List Sector -> H.Html Msg
viewSectors options sectors =
    H.div []
        [ H.text "Sectors"
        , Button.addButton (Button.defaultOptions |> Button.withMsg (OpenSectorForm Nothing))
        , H.div []
            (List.map
                (\sector ->
                    H.div
                        [ A.id <| "sector-" ++ String.fromInt sector.id
                        , A.css [ TW.border, TW.border_solid, TW.py_4 ]
                        , E.onClick Message.Dummy
                        ]
                        [ viewSectorRow { sector = sector, selected = False } ]
                )
                sectors
            )
        ]


viewSectorRow : { sector : Sector, selected : Bool } -> Html Msg
viewSectorRow { sector, selected } =
    H.div
        [ A.css [ TW.flex ]
        ]
        [ H.div [ A.css [] ] [ H.text <| sector.name ]
        , H.button [ E.onClick <| OpenSectorForm (Just sector) ] [ H.text "edit" ]
        , H.button [ E.onClick <| DeleteSectorRequested sector ] [ H.text "delete" ]
        ]



-- Utilities


filterSectors : Model -> List Sector
filterSectors model =
    case model.sectorsPageModel.selectedArea of
        Nothing ->
            Utilities.dictToList model.sectors

        Just area ->
            DU.filterSectorsByAreaId area.id (Utilities.dictToList model.sectors)
