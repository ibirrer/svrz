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


-----------------------------------
-- MODEL --------------------------
-----------------------------------


type Gender
    = Male
    | Female


type alias RankingEntry =
    { rank : Int
    , team : String
    , teamId : Int
    , gamesPlayed : Int
    , setrate :
        { won : Int
        , lost : Int
        }
    , setquotient : Float
    , ballrate :
        { won : Int
        , lost : Int
        }
    , ballquotient : Float
    , points : Int
    }


type alias League =
    { id : String
    , name : String
    , shortName : String
    , gender : Gender
    }


type SortOrder
    = Asc
    | Desc


type alias Model =
    { league : League
    , ranking : List RankingEntry
    , games : {}
    , nextRankingSortOrder : SortOrder
    , leagueData : String
    , jsData : String
    }


initialModel : Model
initialModel =
    { league =
        { id = "9446"
        , name = "Zürimeisterschaft ZM1"
        , shortName = "Zm H1"
        , gender = Male
        }
    , leagueData = "not loaded"
    , jsData = "not loaded"
    , ranking =
        [ { rank = 3
          , team = "Raz Faz"
          , teamId = 1
          , gamesPlayed = 2
          , setrate =
                { won = 6
                , lost = 5
                }
          , setquotient = 2.55
          , ballrate =
                { won = 300
                , lost = 200
                }
          , ballquotient = 2.66
          , points = 10
          }
        , { rank = 2
          , team = "VBC Kanti Limmattal 3"
          , teamId = 2
          , gamesPlayed = 2
          , setrate =
                { won = 6
                , lost = 5
                }
          , setquotient = 2.01
          , ballrate =
                { won = 200
                , lost = 200
                }
          , ballquotient = 2
          , points = 8
          }
        , { rank = 4
          , team = "Einsiedeln"
          , teamId = 4
          , gamesPlayed = 2
          , setrate =
                { won = 6
                , lost = 5
                }
          , setquotient = 2.01
          , ballrate =
                { won = 200
                , lost = 200
                }
          , ballquotient = 2
          , points = 5
          }
        ]
    , games = {}
    , nextRankingSortOrder = Asc
    }



---------------------------------------------------------------
-- UPDATE -----------------------------------------------------
---------------------------------------------------------------


type Action
    = NoOp
    | Sort
    | Delete Int
    | GetFromSvrz
    | LegueDataReceived (Maybe String)
    | JsReceived String


sortRanking : Model -> Model
sortRanking model =
    let
        sortedRanking =
            List.sortBy .rank model.ranking
    in
        case model.nextRankingSortOrder of
            Asc ->
                { model
                    | ranking = sortedRanking
                    , nextRankingSortOrder = Desc
                }

            Desc ->
                { model
                    | ranking = List.reverse sortedRanking
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

        Delete teamId ->
            let
                remainingEntries =
                    List.filter (\e -> e.teamId /= teamId) model.ranking
            in
                ( { model | ranking = remainingEntries }
                , Effects.none
                )

        GetFromSvrz ->
            ( { model | leagueData = "loading..." }
            , getLeagueData model.league.id
            )

        LegueDataReceived maybeResult ->
            ( { model | leagueData = Maybe.withDefault "error" maybeResult }
            , Effects.none
            )

        JsReceived data ->
            ( { model | jsData = data }
            , Effects.none
            )



---------------------------------------------------------------
-- VIEW -------------------------------------------------------
---------------------------------------------------------------


view : Address Action -> Model -> Html
view address model =
    div
        []
        [ pageHeader model.league
        , rankingTable address model.ranking
        , sortButton address model.nextRankingSortOrder
        , getFromSvrzButton address
        , dataFromJs model.jsData
        , leagueDataItem model.leagueData
        , hr [] []
        , pageFooter
        ]


dataFromJs data =
    div
        []
        [ h2 [] [ text "js data" ]
        , pre [] [ text data ]
        ]


leagueDataItem data =
    div
        []
        [ h2 [] [ text "svrz data" ]
        , pre [] [ text data ]
        ]


sortButton : Address Action -> SortOrder -> Html
sortButton address nextRankingSortOrder =
    if nextRankingSortOrder == Asc then
        button [ onClick address Sort ] [ text "Sort (v)" ]
    else
        button [ onClick address Sort ] [ text "Sort (^)" ]


getFromSvrzButton address =
    button [ onClick address GetFromSvrz ] [ text "Get data from svrz" ]


pageHeader : League -> Html
pageHeader league =
    h1 [] [ text league.name ]


rankingHeaderRow : Html
rankingHeaderRow =
    tr
        []
        [ th [] [ text "Rang" ]
        , th [] [ text "Team" ]
        , th [] [ text "Punkte" ]
        , th [] [ text "Action" ]
        ]


rankingRow : Address Action -> RankingEntry -> Html
rankingRow address rankingEntry =
    tr
        []
        [ td [] [ text (toString rankingEntry.rank) ]
        , td [] [ text rankingEntry.team ]
        , td [] [ text (toString rankingEntry.points) ]
        , td
            []
            [ button
                [ onClick address (Delete rankingEntry.teamId) ]
                [ text "Delete" ]
            ]
        ]


sumRankingPoints : List RankingEntry -> Int
sumRankingPoints ranking =
    List.map .points ranking
        |> List.sum


rankingTotalRow : List RankingEntry -> Html
rankingTotalRow ranking =
    tr
        []
        [ td [] [ text "Total" ]
        , td [] [ text "" ]
        , td [] [ ranking |> sumRankingPoints |> toString |> text ]
        , td [] [ text "" ]
        ]


rankingTable : Address Action -> List RankingEntry -> Html
rankingTable address ranking =
    table
        []
        ([ rankingHeaderRow ]
            ++ List.map (rankingRow address) ranking
            ++ [ rankingTotalRow ranking ]
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
        { init = ( (sortRanking initialModel), Effects.none )
        , update = update
        , view = view
        , inputs =
            [ (Signal.map (\str -> JsReceived str) loadData) ]
        }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port loadData : Signal String
