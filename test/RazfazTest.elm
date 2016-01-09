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
        ]


main : Element
main =
    elementRunner tests
