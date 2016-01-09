module Util (dateShortString) where

import Date
import String


dateShortString : String -> String
dateShortString dateAsString =
    case Date.fromString dateAsString of
        Ok date ->
            (Date.dayOfWeek date |> shortDayOfWeek)
                ++ " "
                ++ (Date.day date |> zeroPad)
                ++ "."
                ++ (Date.month date |> monthToInt |> zeroPad)

        Err err ->
            "n/a"


zeroPad : Int -> String
zeroPad number =
    String.padLeft 2 '0' (toString number)


monthToInt : Date.Month -> Int
monthToInt m =
    case m of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


shortDayOfWeek : Date.Day -> String
shortDayOfWeek dow =
    case dow of
        Date.Mon ->
            "MO"

        Date.Tue ->
            "DI"

        Date.Wed ->
            "MI"

        Date.Thu ->
            "DO"

        Date.Fri ->
            "FR"

        Date.Sat ->
            "SA"

        Date.Sun ->
            "SO"
