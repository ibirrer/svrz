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


resultToStyle : Game -> String
resultToStyle game =
    if game.homeTeam == "Raz Faz" then
        case game.result of
            Just g ->
                if g.home > g.away then
                    "won"
                else
                    "lost"

            Nothing ->
                ""
    else if game.awayTeam == "Raz Faz" then
        case game.result of
            Just g ->
                if g.home > g.away then
                    "lost"
                else
                    "won"

            Nothing ->
                ""
    else
        ""


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
        , td [ class (resultToStyle game) ] [ text (gameResultAsString game.result) ]
        ]


view : Games -> Html
view games =
    table [] (List.map row games)
