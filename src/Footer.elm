module Footer (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Html
view =
    footer
        []
        [ div
            [ class "left" ]
            [ text "Daten von", a [ href "http://www.svrz.ch" ] [ text " http://www.svrz.ch" ] ]
        , div
            [ class "right" ]
            [ a [ class "icon-github", href "https://github.com/ibirrer/svrz" ] [ text "Fork on Github" ] ]
        ]
