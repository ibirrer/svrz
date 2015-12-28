module Razfaz (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import String exposing (toUpper, repeat, trimRight)
import StartApp
import Debug
import Signal exposing (Address)
import Effects exposing (Effects)
import Http
import Task


-- local imports

import GamesTable


-----------------------------------
-- MODEL --------------------------
-----------------------------------


type alias RankingEntry =
    { rank : Int
    , team : String
    , games : Int
    , ballquotient : Float
    , points : Int
    }


type alias GameResult =
    { home : Int
    , away : Int
    }


type alias Game =
    { id : Int
    , team : String
    , teamId : Int
    , date : String
    , time : String
    , opponent : String
    , opponentId : Int
    , result : Maybe GameResult
    }


type alias LeagueInfo =
    { leagueId : String
    , games : List Game
    , ranking : List RankingEntry
    }


type alias Model =
    { leagueInfo : LeagueInfo
    , scrapeLeagueFromHtml : Maybe String
    , loadLeagueInfo : Maybe String
    }


initialModel : Model
initialModel =
    { leagueInfo =
        { leagueId = "invalid"
        , games = []
        , ranking = []
        }
    , scrapeLeagueFromHtml = Nothing
    , loadLeagueInfo = Nothing
    }



---------------------------------------------------------------
-- UPDATE -----------------------------------------------------
---------------------------------------------------------------


type Action
    = NoOp
    | LegueDataReceived (Maybe String)
    | JsReceived LeagueInfo
    | GetFromPouchDb String


getLeagueData : String -> Effects Action
getLeagueData leagueId =
    let
        url = "https://crossorigin.me/http://www.svrz.ch/index.php?id=73&nextPage=1&group_ID=" ++ leagueId
    in
        Http.getString url
            |> Task.toMaybe
            |> Task.map LegueDataReceived
            |> Effects.task


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        NoOp ->
            ( model, Effects.none )

        GetFromPouchDb leagueId ->
            ( { model | loadLeagueInfo = Just leagueId }
            , getLeagueData (leagueId)
            )

        LegueDataReceived maybeResult ->
            ( { model
                | scrapeLeagueFromHtml = maybeResult
                , loadLeagueInfo = Nothing
              }
            , Effects.none
            )

        JsReceived newLeagueInfo ->
            let
                leagueInfo = model.leagueInfo
            in
                ( { model
                    | leagueInfo = newLeagueInfo
                    , scrapeLeagueFromHtml = Nothing
                  }
                , Effects.none
                )



---------------------------------------------------------------
-- VIEW -------------------------------------------------------
---------------------------------------------------------------


mapToGamesResultModel : List Game -> GamesTable.Games
mapToGamesResultModel games =
    games
        |> List.filter (\g -> g.team == "Raz Faz" || g.opponent == "Raz Faz")
        |> List.map
            (\g ->
                { date = g.date
                , homeTeam = g.team
                , awayTeam = g.opponent
                , result = g.result
                }
            )


view : Address Action -> Model -> Html
view address model =
    div
        []
        [ pageHeader
        , h2 [] [ text "Rangliste" ]
        , rankingTable address model.leagueInfo.ranking
        , h2 [] [ text "Spiele" ]
        , (GamesTable.view (mapToGamesResultModel model.leagueInfo.games))
        ]


pageHeader : Html
pageHeader =
    h1 [] [ text "ZM Herren 1" ]


rankingHeaderRow : Html
rankingHeaderRow =
    tr
        []
        [ th [] [ text "" ]
        , th [] [ text "" ]
        , th [] [ text "BQ" ]
        , th [] [ text "S" ]
        , th [] [ text "P" ]
        ]


rankingRow : Address Action -> RankingEntry -> Html
rankingRow address rankingEntry =
    let
        rankingStyle =
            if rankingEntry.team == "Raz Faz" then
                [ ( "font-weight", "bold" ) ]
            else
                []
    in
        tr
            [ style rankingStyle ]
            [ td [] [ text ((toString rankingEntry.rank) ++ ".") ]
            , td [] [ text rankingEntry.team ]
            , td [ class "number" ] [ text (toString rankingEntry.ballquotient) ]
            , td [ class "number" ] [ text (toString rankingEntry.games) ]
            , td [ class "number" ] [ text (toString rankingEntry.points) ]
            ]


rankingTable : Address Action -> List RankingEntry -> Html
rankingTable address ranking =
    table
        []
        ([ rankingHeaderRow ]
            ++ List.map (rankingRow address) ranking
        )



---------------------------------------------------------------
-- WIRING -----------------------------------------------------
---------------------------------------------------------------


app : StartApp.App Model
app =
    StartApp.start
        { init = ( initialModel, Effects.task (Task.succeed (GetFromPouchDb "9446")) )
        , update = update
        , view = view
        , inputs =
            [ (Signal.map (\leagueInfo -> JsReceived leagueInfo) loadData) ]
        }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port loadData : Signal LeagueInfo
port scrapeSvrz : Signal String
port scrapeSvrz =
    Signal.filterMap (\m -> m.scrapeLeagueFromHtml) "initial" app.model


port loadLeagueInfo : Signal String
port loadLeagueInfo =
    Signal.filterMap (\m -> m.loadLeagueInfo) "initial" app.model
