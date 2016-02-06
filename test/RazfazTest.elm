module RazfazTest (..) where

import Razfaz
import String
import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import Util exposing (..)


tests : Test
tests =
    suite
        "A Test Suite"
        [ test
            "dateShortString"
            (assertEqual
                "DI 05.01"
                (dateShortString "2016-01-05T10:20:30Z")
            )
        , test
            "dateShortString"
            (assertEqual
                "MI 21.12"
                (dateShortString "2016-12-21T10:20:30Z")
            )
        , test
            "zero pad"
            (assertEqual
                "00"
                (zeroPad 0)
            )
        , test
            "zero pad"
            (assertEqual
                "05"
                (zeroPad 5)
            )
        , testMergeModel
        , matchInt
        , testMergeLeagueInfoGames
        ]


testMergeLeagueInfoGames =
    let
        gamesWithDetails =
            [ { id = 1
              , setsResults = Just { home = [ 25, 25, 25 ], away = [ 10, 14, 18 ] }
              , gym =
                    Just
                        { name = "Turnhalle"
                        , map = "http://map.com?q=Turnhalle"
                        }
              }
            ]

        gamesWithouthDetails =
            [ { id = 1
              , setsResults = Nothing
              , gym = Nothing
              }
            ]

        expectedGames =
            [ { id = 1
              , setsResults = Just { home = [ 25, 25, 25 ], away = [ 10, 14, 18 ] }
              , gym =
                    Just
                        { name = "Turnhalle"
                        , map = "http://map.com?q=Turnhalle"
                        }
              }
            ]
    in
        test
            "merge"
            (assertEqual
                expectedGames
                (Razfaz.mergeLeagueInfoGames gamesWithDetails gamesWithouthDetails)
            )


matchInt =
    suite
        "matchInt"
        [ test
            "finds exact match"
            (assertEqual
                (Just 23)
                (Razfaz.matchInt "games/([0-9]*)" "games/23")
            )
        , test
            "return nothin if more than one match"
            (assertEqual
                (Nothing)
                (Razfaz.matchInt "games/([0-9]*)/([0-9]*)" "games/33/23")
            )
        ]


testMergeModel =
    let
        games =
            [ { id = 1
              , setsResults = Nothing
              , gym = Nothing
              }
            ]

        gameDetails =
            [ { gameId = 1
              , setsResults = Just { home = [ 25, 25, 25 ], away = [ 10, 14, 18 ] }
              , gym =
                    { name = "Turnhalle"
                    , map = "http://map.com?q=Turnhalle"
                    }
              }
            ]

        expectedGames =
            [ { id = 1
              , setsResults = Just { home = [ 25, 25, 25 ], away = [ 10, 14, 18 ] }
              , gym =
                    Just
                        { name = "Turnhalle"
                        , map = "http://map.com?q=Turnhalle"
                        }
              }
            ]
    in
        test
            "merge game details into league info"
            (assertEqual
                expectedGames
                (Razfaz.mergeGameDetails games gameDetails)
            )


main : Element
main =
    elementRunner tests
