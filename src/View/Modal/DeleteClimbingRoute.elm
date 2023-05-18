module View.Modal.DeleteClimbingRoute exposing (view)

import Data exposing (ClimbingRoute)
import Html.Styled as H exposing (Html)
import Html.Styled.Events as E
import Utilities


view : ClimbingRoute -> (ClimbingRoute -> msg) -> Html msg
view route msg =
    H.div []
        [ H.h2 []
            [ H.text <| Utilities.stringFromList [ "Delete \"", route.name, "\" " ] ]
        , H.button [ E.onClick <| msg route ] [ H.text "confirm" ]
        ]
