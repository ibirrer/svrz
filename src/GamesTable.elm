module GamesTable (Games, view) where

import Html exposing (..)
import Html.Attributes exposing (..)


-- Model


type alias GameResult =
    { home : Int
    , away : Int
    }


type alias Game =
    { date : String
    , homeTeam : String
    , awayTeam : String
    , result : Maybe GameResult
    }


type alias Games =
    List Game



-- View


gameResultAsString : Maybe GameResult -> String
gameResultAsString gameResult =
    case gameResult of
        Just g ->
            toString g.home ++ " : " ++ toString g.away

        Nothing ->
            "n/a"


row : Game -> Html
row game =
    tr
        []
        [ td [] [ text (game.homeTeam) ]
        , td [] [ text (game.awayTeam) ]
        , td [] [ text (gameResultAsString game.result) ]
        ]


view : Games -> Html
view games =
    table [] (List.map row games)
