module Razfaz (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, filter)
import String exposing (toUpper, repeat, trimRight, join)
import Dict exposing (Dict)
import Regex exposing (regex)
import Signal exposing (Address)
import Effects exposing (Effects)
import Http
import Task
import Util
import Debug
import Result


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


type alias SetResult =
    { home : List Int
    , away : List Int
    }


type alias Gym =
    { name : String
    , map : String
    }


type alias Game =
    { id : Int
    , team : String
    , teamId : Int
    , datetime : String
    , opponent : String
    , opponentId : Int
    , result : Maybe GameResult
    , setsResults : Maybe SetResult
    , gym : Maybe Gym
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
    , upsertLeagueInfo : Maybe LeagueInfo
    , teamId : Int
    , pageType : PageType
    }


type alias GameDetail =
    { gameId : Int
    , setsResults : Maybe SetResult
    , gym : Gym
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
    , upsertLeagueInfo = Nothing
    , teamId = defaultTeam
    , pageType = TeamPage
    }



---------------------------------------------------------------
-- UPDATE -----------------------------------------------------
---------------------------------------------------------------


type PageType
    = TeamPage
    | GamePage (Maybe Int)


type Action
    = NoOp
    | GotLeagueHtmlFromSvrz (Maybe String)
    | ScrapedLeagueHtml LeagueInfo
    | GotFromPouchDb LeagueInfo
    | ErrorGetFromCouchDb String
    | GotGamesDetailsHtmlFromSvrz (Maybe (List String))
    | GetFromPouchDb String
    | ScrapedGamesDetailsFromHtml (List GameDetail)
    | UrlHashChanged String
    | UpsertedLeagueInfo


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
        urls = gameIds |> List.map gameDetailUrl

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


type alias AbstractGame a =
    { a | id : Int, setsResults : Maybe SetResult, gym : Maybe Gym }


mergeGames : AbstractGame a -> Maybe GameDetail -> AbstractGame a
mergeGames game gameDetail =
    case gameDetail of
        Just gameDetail' ->
            { game
                | setsResults = gameDetail'.setsResults
                , gym = Just gameDetail'.gym
            }

        Nothing ->
            game


getGameDetail : List GameDetail -> Int -> Maybe GameDetail
getGameDetail gameDetails gameId =
    gameDetails
        |> List.filter (\g -> g.gameId == gameId)
        |> List.head


mergeGameDetails : List (AbstractGame a) -> List GameDetail -> List (AbstractGame a)
mergeGameDetails games gameDetails =
    games |> List.map (\game -> mergeGames game (getGameDetail gameDetails game.id))


mergeGameDetailsWithLeagueInfo : LeagueInfo -> List GameDetail -> LeagueInfo
mergeGameDetailsWithLeagueInfo leagueInfo gamesDetails =
    { leagueInfo | games = mergeGameDetails leagueInfo.games gamesDetails }


getGame : List (AbstractGame a) -> Int -> Maybe (AbstractGame a)
getGame games gameId =
    games
        |> filter (\game -> game.id == gameId)
        |> List.head


mergeLeagueInfoGame : Maybe (AbstractGame a) -> AbstractGame a -> AbstractGame a
mergeLeagueInfoGame srcGame destGame =
    case srcGame of
        Just srcGame' ->
            { destGame
                | setsResults = srcGame'.setsResults
                , gym = srcGame'.gym
            }

        Nothing ->
            destGame



-- merges games details (setsResults and gym) from `src` into `dest`


mergeLeagueInfoGames : List (AbstractGame a) -> List (AbstractGame a) -> List (AbstractGame a)
mergeLeagueInfoGames src dest =
    dest |> map (\destGame -> mergeLeagueInfoGame (getGame src destGame.id) destGame)


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
                | getFromCouchDb = Nothing
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
            let
                currentLeagueInfo = model.leagueInfo

                updateLeagueInfo =
                    if List.isEmpty currentLeagueInfo.games then
                        newLeagueInfo
                    else
                        { currentLeagueInfo
                            | ranking = newLeagueInfo.ranking
                            , games = mergeLeagueInfoGames currentLeagueInfo.games newLeagueInfo.games
                        }
            in
                ( { model
                    | leagueInfo = updateLeagueInfo
                    , scrapeLeagueFromHtml = Nothing
                  }
                , getGamesDetailsHtmlFromSvrz (newLeagueInfo.games |> List.map .id)
                )

        GotGamesDetailsHtmlFromSvrz gamesDetailsHtml ->
            ( { model | scrapeGamesDetailsFromHtml = gamesDetailsHtml }
            , Effects.none
            )

        ScrapedGamesDetailsFromHtml gamesDetails ->
            let
                modelWithGameDetails =
                    { model
                        | leagueInfo = mergeGameDetailsWithLeagueInfo model.leagueInfo gamesDetails
                        , scrapeGamesDetailsFromHtml = Nothing
                    }
            in
                ( { modelWithGameDetails
                    | upsertLeagueInfo = Just modelWithGameDetails.leagueInfo
                  }
                , Effects.task (Task.succeed (UpsertedLeagueInfo))
                )

        UpsertedLeagueInfo ->
            ( { model | upsertLeagueInfo = Nothing }
            , Effects.none
            )

        UrlHashChanged hash ->
            let
                ( teamId, pageType ) =
                    if Regex.contains (regex "^#teams/[0-9]{5}$") hash then
                        ( getTeamIdFromHash (hash), TeamPage )
                    else if Regex.contains (regex "^#games/[0-9]*$") hash then
                        ( defaultTeam, GamePage (matchInt "^#games/([0-9]*)$" hash) )
                    else
                        ( defaultTeam, TeamPage )

                leagueId =
                    Maybe.withDefault defaultLeague (Dict.get teamId teamToLeagueMapping)

                effect =
                    if leagueId /= model.leagueInfo.leagueId then
                        Effects.task (Task.succeed (GetFromPouchDb leagueId))
                    else
                        Effects.none

                leagueInfo = model.leagueInfo

                leagueInfo' = { leagueInfo | leagueId = leagueId }
            in
                ( { model
                    | teamId = teamId
                    , leagueInfo = leagueInfo'
                    , pageType = pageType
                  }
                , effect
                )


flatMap : (a -> Maybe b) -> Maybe a -> Maybe b
flatMap callback maybe =
    Maybe.andThen maybe callback



-- matches the first integer in the given regular expression


matchInt : String -> String -> Maybe Int
matchInt regex string =
    let
        match = Regex.find Regex.All (Regex.regex regex) string
    in
        case match of
            head :: [] ->
                case head.submatches of
                    head :: [] ->
                        case head of
                            Just s' ->
                                String.toInt s' |> Result.toMaybe

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing

            _ ->
                Nothing



---------------------------------------------------------------
-- VIEW -------------------------------------------------------
---------------------------------------------------------------


setResultView game =
    case game.setsResults of
        Just result ->
            div
                []
                [ p [] [ result.home |> map toString |> join " " |> text, span [] [ text (" - " ++ game.team) ] ]
                , p [] [ result.away |> map toString |> join " " |> text, span [] [ text (" - " ++ game.opponent) ] ]
                ]

        Nothing ->
            p [] [ text "n/a" ]


gymView gym =
    case gym of
        Just gym' ->
            p [] [ a [ href gym'.map ] [ text gym'.name ] ]

        Nothing ->
            p [] [ text "Keine Angaben zur Turnhalle" ]


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

        GamePage gameId ->
            case gameId of
                Just gameId' ->
                    let
                        game =
                            model.leagueInfo.games
                                |> List.filter (\g -> g.id == gameId')
                                |> List.head
                    in
                        case game of
                            Just game' ->
                                div
                                    []
                                    [ pageHeader
                                    , h2 [] [ text "Spiele Details" ]
                                    , setResultView game'
                                    , gymView game'.gym
                                    ]

                            Nothing ->
                                div [] [ text "Spiel nicht gefunden" ]

                Nothing ->
                    div [] [ text "Spiel nicht gefunden" ]


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

        opponent =
            text
                (if model.teamId == game.teamId then
                    game.opponent
                 else
                    game.team
                )
    in
        tr
            []
            [ td [] [ text (homeAwayShortString model game) ]
            , td [] [ text (Util.dateShortString game.datetime) ]
            , case game.gym of
                Just gym' ->
                    td
                        []
                        [ a
                            [ href ("#games/" ++ toString game.id) ]
                            [ opponent ]
                        ]

                Nothing ->
                    td [] [ opponent ]
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
