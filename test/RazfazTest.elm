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
