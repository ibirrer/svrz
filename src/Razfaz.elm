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


type SortOrder
    = Asc
    | Desc


type alias Model =
    { leagueInfo : LeagueInfo
    , nextRankingSortOrder : SortOrder
    , scrapeLeagueFromHtml : Maybe String
    }


initialModel : Model
initialModel =
    { leagueInfo =
        { leagueId = "9446"
        , games = []
        , ranking = []
        }
    , nextRankingSortOrder = Asc
    , scrapeLeagueFromHtml = Nothing
    }



---------------------------------------------------------------
-- UPDATE -----------------------------------------------------
---------------------------------------------------------------


type Action
    = NoOp
    | Sort
    | GetFromSvrz
    | LegueDataReceived (Maybe String)
    | JsReceived LeagueInfo


sortRanking : Model -> Model
sortRanking model =
    let
        sortedRanking =
            List.sortBy .rank model.leagueInfo.ranking

        leagueInfo =
            model.leagueInfo

        sortedLeagueInfo =
            case model.nextRankingSortOrder of
                Asc ->
                    { leagueInfo | ranking = sortedRanking }

                Desc ->
                    { leagueInfo | ranking = List.reverse sortedRanking }
    in
        case model.nextRankingSortOrder of
            Asc ->
                { model
                    | leagueInfo = sortedLeagueInfo
                    , nextRankingSortOrder = Desc
                }

            Desc ->
                { model
                    | leagueInfo = sortedLeagueInfo
                    , nextRankingSortOrder = Asc
                }


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

        Sort ->
            ( (sortRanking model), Effects.none )

        GetFromSvrz ->
            ( model
            , getLeagueData (model.leagueInfo.leagueId)
            )

        LegueDataReceived maybeResult ->
            ( { model
                | scrapeLeagueFromHtml = maybeResult
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


mapToGamesResultModel games =
    List.map
        (\g ->
            { date = g.date
            , homeTeam = g.team
            , awayTeam = g.opponent
            , result = g.result
            }
        )
        games


view : Address Action -> Model -> Html
view address model =
    div
        []
        [ pageHeader
        , h2 [] [ text "Rangliste" ]
        , rankingTable address model.leagueInfo.ranking
        , h2 [] [ text "Spiele" ]
        , (GamesTable.view (mapToGamesResultModel model.leagueInfo.games))
        , sortButton address model.nextRankingSortOrder
        , getFromSvrzButton address
        , hr [] []
        , pageFooter
        ]


sortButton : Address Action -> SortOrder -> Html
sortButton address nextRankingSortOrder =
    if nextRankingSortOrder == Asc then
        button [ onClick address Sort ] [ text "Sort (v)" ]
    else
        button [ onClick address Sort ] [ text "Sort (^)" ]


getFromSvrzButton address =
    button [ onClick address GetFromSvrz ] [ text "Get data from svrz" ]


pageHeader : Html
pageHeader =
    h1 [] [ text "ZM Herren 1" ]


rankingHeaderRow : Html
rankingHeaderRow =
    tr
        []
        [ th [] [ text "" ]
        , th [] [ text "" ]
        , th [] [ text "Ballquotient" ]
        , th [] [ text "Punkte" ]
        ]


rankingRow : Address Action -> RankingEntry -> Html
rankingRow address rankingEntry =
    tr
        []
        [ td [] [ text ((toString rankingEntry.rank) ++ ".") ]
        , td [] [ text rankingEntry.team ]
        , td [] [ text (toString rankingEntry.ballquotient) ]
        , td [] [ text (toString rankingEntry.points) ]
        ]


rankingTable : Address Action -> List RankingEntry -> Html
rankingTable address ranking =
    table
        []
        ([ rankingHeaderRow ]
            ++ List.map (rankingRow address) ranking
        )


pageFooter : Html
pageFooter =
    footer
        []
        [ a [ href "http://razfaz.there.ch" ] [ text "Raz Faz" ]
        , text " by TV Wollishofen."
        ]



---------------------------------------------------------------
-- WIRING -----------------------------------------------------
---------------------------------------------------------------


app : StartApp.App Model
app =
    StartApp.start
        { init = ( initialModel, getLeagueData (initialModel.leagueInfo.leagueId) )
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
