module Main (..) where

import Razfaz exposing (..)
import StartApp
import Task
import Effects exposing (Effects)
import Html exposing (Html)


app : StartApp.App Model
app =
    StartApp.start
        { init = ( initialModel, Effects.none )
        , update = update
        , view = view
        , inputs =
            [ (Signal.map (\leagueInfo -> GotFromPouchDb leagueInfo) setLeagueData)
            , (Signal.map (\error -> ErrorGetFromCouchDb error) errorGetFromPouchDb)
            , (Signal.map (\leagueInfo -> ScrapedLeagueHtml leagueInfo) scrapedLeagueHtml)
            , (Signal.map (\gamesDetails -> ScrapedGamesDetailsFromHtml gamesDetails) scrapedGamesDetailsFromHtml)
            , (Signal.map (\hash -> UrlHashChanged hash) urlHashChanged)
            ]
        }


main : Signal Html
main =
    app.html



-- start app port


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks



-- inbound ports (functions that can be called from js)


port errorGetFromPouchDb : Signal String
port setLeagueData : Signal LeagueInfo
port scrapedLeagueHtml : Signal LeagueInfo
port scrapedGamesDetailsFromHtml : Signal (List GameDetail)
port urlHashChanged : Signal String



-- outbound ports (functions that can be called from elm)


port scrapeLeagueFromHtml : Signal String
port scrapeLeagueFromHtml =
    Signal.filterMap (\m -> m.scrapeLeagueFromHtml) "initial" app.model


port scrapeGamesDetailsFromHtml : Signal (List String)
port scrapeGamesDetailsFromHtml =
    Signal.filterMap (\m -> m.scrapeGamesDetailsFromHtml) [] app.model


port getFromCouchDb : Signal String
port getFromCouchDb =
    Signal.filterMap (\m -> m.getFromCouchDb) "initial" app.model
