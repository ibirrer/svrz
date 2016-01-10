module Razfaz (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import String exposing (toUpper, repeat, trimRight)
import Dict exposing (Dict)
import StartApp
import Regex exposing (regex)
import Signal exposing (Address)
import Effects exposing (Effects)
import Http
import Task
import Util


-----------------------------------
-- MODEL --------------------------
-----------------------------------


type alias RankingEntry =
    { rank : Int
    , team : String
    , teamId : Int
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
    , datetime : String
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
    , teamId : Int
    }


teamToLeagueMapping : Dict Int String
teamToLeagueMapping =
    Dict.fromList
        [ ( 26178, "9446" )
        , ( 25739, "9446" )
        , ( 25649, "9446" )
        , ( 25696, "9446" )
        , ( 25745, "9446" )
        ]


defaultLeague : String
defaultLeague =
    "9446"


defaultTeam : Int
defaultTeam =
    25649


initialModel : Model
initialModel =
    { leagueInfo =
        { leagueId = "invalid"
        , games = []
        , ranking = []
        }
    , scrapeLeagueFromHtml = Nothing
    , loadLeagueInfo = Nothing
    , teamId = defaultTeam
    }



---------------------------------------------------------------
-- UPDATE -----------------------------------------------------
---------------------------------------------------------------


type Action
    = NoOp
    | LegueDataReceived (Maybe String)
    | JsReceived LeagueInfo
    | GetFromPouchDb String
    | UrlHashChanged String


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
            ( model
            , Effects.none
            )

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

        UrlHashChanged hash ->
            let
                teamId =
                    if Regex.contains (regex "^#teams/[0-9]{5}$") hash then
                        let
                            teamIdString = String.dropLeft 7 hash
                        in
                            case String.toInt teamIdString of
                                Err msg ->
                                    defaultTeam

                                Ok teamId' ->
                                    teamId'
                    else
                        defaultTeam

                leagueId =
                    Maybe.withDefault defaultLeague (Dict.get teamId teamToLeagueMapping)
            in
                ( { model | teamId = teamId }
                , Effects.task (Task.succeed (GetFromPouchDb leagueId))
                )



---------------------------------------------------------------
-- VIEW -------------------------------------------------------
---------------------------------------------------------------


view : Address Action -> Model -> Html
view address model =
    div
        []
        [ pageHeader
        , h2 [] [ text "Rangliste" ]
        , rankingTable address model
        , h2 [] [ text "Spiele" ]
        , gamesTable model
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


rankingRow : Address Action -> Int -> RankingEntry -> Html
rankingRow address teamId rankingEntry =
    let
        rankingStyle =
            if rankingEntry.teamId == teamId then
                [ ( "font-weight", "bold" ) ]
            else
                []

        teamUrl = "#teams/" ++ toString rankingEntry.teamId
    in
        tr
            [ style rankingStyle ]
            [ td [] [ text ((toString rankingEntry.rank) ++ ".") ]
            , td [] [ a [ href teamUrl ] [ text rankingEntry.team ] ]
            , td [ class "number" ] [ text (toString rankingEntry.ballquotient) ]
            , td [ class "number" ] [ text (toString rankingEntry.games) ]
            , td [ class "number" ] [ text (toString rankingEntry.points) ]
            ]


rankingTable : Address Action -> Model -> Html
rankingTable address model =
    table
        []
        ([ rankingHeaderRow ]
            ++ List.map (rankingRow address model.teamId) model.leagueInfo.ranking
        )


type GameResultState
    = WON Int Int
    | LOST Int Int
    | NOT_PLAYED


getGameResultState : Game -> Int -> GameResultState
getGameResultState game teamId =
    if game.teamId == teamId then
        case game.result of
            Just g ->
                if g.home > g.away then
                    WON g.home g.away
                else
                    LOST g.home g.away

            Nothing ->
                NOT_PLAYED
    else if game.opponentId == teamId then
        case game.result of
            Just g ->
                if g.home > g.away then
                    LOST g.away g.home
                else
                    WON g.away g.home

            Nothing ->
                NOT_PLAYED
    else
        NOT_PLAYED


resultToStyle : GameResultState -> String
resultToStyle gameResultState =
    case gameResultState of
        WON h a ->
            "won"

        LOST h a ->
            "lost"

        NOT_PLAYED ->
            ""


gameResultAsString : GameResultState -> String
gameResultAsString gameResultState =
    case gameResultState of
        WON h a ->
            toString h ++ " : " ++ toString a

        LOST h a ->
            toString h ++ " : " ++ toString a

        NOT_PLAYED ->
            "n/a"


homeAwayShortString : Model -> Game -> String
homeAwayShortString model game =
    if model.teamId == game.teamId then
        "H"
    else
        "A"


gamesHeaderRow : Html
gamesHeaderRow =
    tr
        []
        [ th [] [ text "" ]
        , th [] [ text "" ]
        , th [ style [ ( "text-align", "left" ) ] ] [ text "Gegner" ]
        , th [] [ text "Resultat" ]
        ]


gamesRow : Model -> Game -> Html
gamesRow model game =
    let
        gameResultState = getGameResultState game model.teamId
    in
        tr
            []
            [ td [] [ text (homeAwayShortString model game) ]
            , td [] [ text (Util.dateShortString game.datetime) ]
            , td
                []
                [ text
                    (if model.teamId == game.teamId then
                        game.opponent
                     else
                        game.team
                    )
                ]
            , td
                [ classList
                    [ ( resultToStyle gameResultState, True )
                    , ( "number", True )
                    ]
                ]
                [ text (gameResultAsString gameResultState) ]
            ]


gamesTable : Model -> Html
gamesTable model =
    let
        filteredGames =
            model.leagueInfo.games
                |> List.filter (\g -> g.teamId == model.teamId || g.opponentId == model.teamId)
    in
        table
            [id "games"]
            ([ gamesHeaderRow ]
                ++ (List.map (gamesRow model) (filteredGames))
            )



---------------------------------------------------------------
-- WIRING -----------------------------------------------------
---------------------------------------------------------------


app : StartApp.App Model
app =
    StartApp.start
        { init = ( initialModel, Effects.none )
        , update = update
        , view = view
        , inputs =
            [ (Signal.map (\leagueInfo -> JsReceived leagueInfo) loadData)
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



-- inbound ports


port loadData : Signal LeagueInfo
port urlHashChanged : Signal String



-- outbound ports


port scrapeSvrz : Signal String
port scrapeSvrz =
    Signal.filterMap (\m -> m.scrapeLeagueFromHtml) "initial" app.model


port loadLeagueInfo : Signal String
port loadLeagueInfo =
    Signal.filterMap (\m -> m.loadLeagueInfo) "initial" app.model
