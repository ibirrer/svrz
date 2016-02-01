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
import Debug


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
    , scrapeGamesDetailsFromHtml : Maybe (List String)
    , getFromCouchDb : Maybe String
    , teamId : Int
    , pageType : PageType
    }


teamToLeagueMapping : Dict Int String
teamToLeagueMapping =
    Dict.fromList
        [ ( 26178, "9446" )
        , ( 25739, "9446" )
        , ( 25649, "9446" )
        , ( 25696, "9446" )
        , ( 25745, "9446" )
        , ( 25687, "9176" )
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
    , scrapeGamesDetailsFromHtml = Nothing
    , getFromCouchDb = Nothing
    , teamId = defaultTeam
    , pageType = TeamPage
    }



---------------------------------------------------------------
-- UPDATE -----------------------------------------------------
---------------------------------------------------------------


type PageType
    = TeamPage
    | GamePage


type Action
    = NoOp
    | GotLeagueHtmlFromSvrz (Maybe String)
    | ScrapedLeagueHtml LeagueInfo
    | GotFromPouchDb LeagueInfo
    | ErrorGetFromCouchDb String
    | GotGamesDetailsHtmlFromSvrz (Maybe (List String))
    | GetFromPouchDb String
    | UrlHashChanged String


getLeagueHtmlFromSvrz : String -> Effects Action
getLeagueHtmlFromSvrz leagueId =
    let
        url = "https://crossorigin.me/http://www.svrz.ch/index.php?id=73&nextPage=1&group_ID=" ++ leagueId
    in
        Http.getString url
            |> Task.toMaybe
            |> Task.map GotLeagueHtmlFromSvrz
            |> Effects.task


gameDetailUrl : Int -> String
gameDetailUrl gameId =
    "https://crossorigin.me/http://www.svrz.ch/index.php?id=73&nextPage=3&game_ID=" ++ (toString gameId)



getGamesDetailsHtmlFromSvrz : List Int -> Effects Action
getGamesDetailsHtmlFromSvrz gameIds =
    let
        urls = gameIds |> List.map gameDetailUrl |> Debug.log "urls"
        tasks = urls |> List.map Http.getString
    in
        Task.sequence tasks
            |> Task.toMaybe
            |> Task.map GotGamesDetailsHtmlFromSvrz
            |> Effects.task


getTeamIdFromHash : String -> Int
getTeamIdFromHash hash =
    let
        teamIdString = String.dropLeft 7 hash
    in
        case String.toInt teamIdString of
            Err msg ->
                defaultTeam

            Ok teamId' ->
                teamId'


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        NoOp ->
            ( model
            , Effects.none
            )

        GetFromPouchDb leagueId ->
            ( { model | getFromCouchDb = Just leagueId }
            , Effects.none
            )


        GotFromPouchDb newLeagueInfo ->
            ( { model
                | leagueInfo = newLeagueInfo
                , getFromCouchDb = Nothing
                }
            , getLeagueHtmlFromSvrz newLeagueInfo.leagueId
            )

        ErrorGetFromCouchDb error ->
            ( { model
                    |  getFromCouchDb = Nothing
              }
            , getLeagueHtmlFromSvrz model.leagueInfo.leagueId
            )


        GotLeagueHtmlFromSvrz leagueHtml ->
            ( { model
                | scrapeLeagueFromHtml = leagueHtml
              }
            , Effects.none
            )

        ScrapedLeagueHtml newLeagueInfo ->
            ( { model
                | leagueInfo = newLeagueInfo
                , scrapeLeagueFromHtml = Nothing
              }
            , getGamesDetailsHtmlFromSvrz(newLeagueInfo.games |> List.map .id)
            )

        GotGamesDetailsHtmlFromSvrz gamesDetailsHtml ->
            ( { model | scrapeGamesDetailsFromHtml = gamesDetailsHtml }
            , Effects.none
            )

        {-
            # Hash Changed
                a. #teams/{teamId} [x]
                    a. leagueId found [x]
                        a. leagueId changed [x]
                            1. Try to load leagueData from PouchDB [x]
                                a. leagueData found [x]
                                    1. Display leagueData (offline first) [x]
                                    2. -> Update leagueData from svrz.ch [x]
                                b. leagueData not found or PouchDB not available
                                    1. -> Update leagueData from svrz.ch [x]
                        b. leagueId unchanged [x]
                            1. do nothing [x]
                    b. leagueId not found
                        1. redirect to #teams/{defaultTeamId}
                b. #games/{gameId}
                c. catch all
                    1. redirect to #teams/{defaultTeamId}

            # Update leagueData from svrz.ch
                1. Check if current data is already up to date (indication
                   could be the data of the first game withouth any results and if all details are available) [ ]

                    a. data is up to date [ ]
                        1. Do nothing [ ]
                    b. data is not up to data [x]
                        1. HTTP GET request to league overview (contains ranking and games) [x]
                            a. Error (data available from PouchDB) [ ]
                                1. Log the error [ ]
                            b. Error (data not available in PouchDB) [ ]
                                1. Display error message in browser. Ideally with the request URL that failed [ ]
                            c. Success [x]
                                1. Scrape leagueInfo (using svrz-scraper) [x]
                                    a. Error (see error cases above) [ ]
                                    b. Success [x]
                                        1. Merge leagueInfo with current leagueInfo (do not overwrite detail data) [ ]
                                        2. -> Update leagueData Details from svrz.ch [x]

            # Update leagueData Details from svrz.ch
                1. HTTP GET request to all detail pages [x]
                    a. Error [ ]
                        1. Log the error [ ]
                    b. Success [ ]
                        1. Scrape all detail pages [ ]
                            a. Error [ ]
                                1. Log error [ ]
                            b. Success [ ]
                                1. Merge detail data with leagueInfo [ ]
                                2. Store data in PouchDB [ ]
                                    a. Error
                                        1. Log error
                                    b. Success
        -}
        UrlHashChanged hash ->
            let
                ( teamId, pageType ) =
                    if Regex.contains (regex "^#teams/[0-9]{5}$") hash then
                        ( getTeamIdFromHash (hash), TeamPage )
                    else if Regex.contains (regex "^#games/[0-9]{5}$") hash then
                        ( defaultTeam, GamePage )
                    else
                        ( defaultTeam, TeamPage )

                leagueId =
                    Maybe.withDefault defaultLeague (Dict.get teamId teamToLeagueMapping) |> Debug.log "leagueId"


                effect =
                    if leagueId /= model.leagueInfo.leagueId then
                        Effects.task (Task.succeed (GetFromPouchDb leagueId))
                    else
                        Effects.none

                leagueInfo = model.leagueInfo
                leagueInfo'  = { leagueInfo | leagueId = leagueId }

            in
                ( { model
                    | teamId = teamId
                    , leagueInfo = leagueInfo'
                    , pageType = pageType
                  }
                , effect
                )



---------------------------------------------------------------
-- VIEW -------------------------------------------------------
---------------------------------------------------------------


view : Address Action -> Model -> Html
view address model =
    case model.pageType of
        TeamPage ->
            div
                []
                [ pageHeader
                , h2 [] [ text "Rangliste" ]
                , rankingTable address model
                , h2 [] [ text "Spiele" ]
                , gamesTable model
                ]

        GamePage ->
            div
                []
                [ pageHeader
                , h2 [] [ text "Game Details" ]
                , h4 [] [ text "im Team" ]
                , h4 [] [ text "Nicht im Team" ]
                , h4 [] [ text "Abgemeldet" ]

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
            [ id "games" ]
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
            [ (Signal.map (\leagueInfo -> GotFromPouchDb leagueInfo) setLeagueData)
            , (Signal.map (\error -> ErrorGetFromCouchDb error) errorGetFromPouchDb)
            , (Signal.map (\leagueInfo -> ScrapedLeagueHtml leagueInfo) scrapedLeagueHtml)
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
