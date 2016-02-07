module Header (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Html
view =
    header
        []
        [ h1 [] [ text "Zürimeisterschaft ZM1" ]
        , h2 [] [ text "Zm H1 - Männer" ]
        ]
