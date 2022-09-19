module Skeleton exposing (..)

import Browser
import FontAwesome.Styles as Icon
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Lazy



-- Config


type alias Details msg =
    { title : String
    , header : List (H.Html msg)
    , warning : Warning
    , kids : List (H.Html msg)
    }


type Warning
    = NoProblems



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title =
        details.title
    , body =
        [ Icon.css
        , H.toUnstyled <|
            H.div []
                [ viewHeader
                , Html.Styled.Lazy.lazy viewWarning details.warning
                , H.map toMsg <|
                    H.div (A.class "center" :: A.style "flex" "1" :: []) details.kids
                ]
        ]
    }


viewHeader : H.Html msg
viewHeader =
    H.div [ A.class "header" ] []


viewWarning : Warning -> H.Html msg
viewWarning warning =
    H.div [ A.class "header-underbar" ] <|
        case warning of
            NoProblems ->
                []
