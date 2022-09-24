module Skeleton exposing (..)

import Browser
import FontAwesome.Styles as Icon
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Lazy
import Navbar
import Session



-- Config


type alias Details msg =
    { title : String
    , warning : Warning
    , kids : List (H.Html msg)
    , session : Session.Model
    }


type Warning
    = NoProblems



-- VIEW


view : (a -> msg) -> (Navbar.Msg -> msg) -> Details a -> Browser.Document msg
view toMsg toNavbarMsg details =
    { title =
        details.title
    , body =
        [ Icon.css
        , H.toUnstyled <|
            H.div []
                [ H.map toNavbarMsg (Navbar.view details.session)
                , Html.Styled.Lazy.lazy viewWarning details.warning
                , H.map toMsg <|
                    H.div (A.class "center" :: A.style "flex" "1" :: []) details.kids
                ]
        ]
    }


viewWarning : Warning -> H.Html msg
viewWarning warning =
    H.div [ A.class "header-underbar" ] <|
        case warning of
            NoProblems ->
                []
