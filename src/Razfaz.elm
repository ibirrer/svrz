module Razfaz (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import String exposing (toUpper, repeat, trimRight)
import StartApp.Simple as StartApp
import Debug
import Signal exposing (Address)


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
    { name : String
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
    }


initialModel : Model
initialModel =
    { league =
        { name = "Zürimeisterschaft ZM1"
        , shortName = "Zm H1"
        , gender = Male
        }
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


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Sort ->
            sortRanking model

        Delete teamId ->
            let
                remainingEntries =
                    List.filter (\e -> e.teamId /= teamId) model.ranking
            in
                { model | ranking = remainingEntries }



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
        , pageFooter
        ]


sortButton : Address Action -> SortOrder -> Html
sortButton address nextRankingSortOrder =
    if nextRankingSortOrder == Asc then
        button [ onClick address Sort ] [ text "Sort (▼)" ]
    else
        button [ onClick address Sort ] [ text "Sort (▲)" ]


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


main : Signal Html
main =
    StartApp.start
        { model =
            sortRanking initialModel
        , view = view
        , update = update
        }
