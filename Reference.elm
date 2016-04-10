module Reference (model, view, update, Action, Model) where

import Html exposing (Html, div, button, text, input, Attribute)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (class)
import Time exposing (timestamp, Time)
import String exposing (length)
import Date
import Date exposing (Month(..), Day(..), Date)
import Signal exposing (Mailbox, mailbox, map2)

--   Model
-- =========
--
-- This is our model. It contains everything we need to generate
-- our output HTML.
--
type alias Model =
    { firstName : String
    , lastName : String
    , time : Time
    }

model : Model
model =
    { firstName = ""
    , lastName = ""
    , time = 0
    }

--   View
-- ========
--
-- This is what we'll display, given our model. inputs get sent to
-- the provided address, which takes them to the update function:
--
view address model =
  div []
    [ div [class "first"]
        [ text "First Name:"
        , input [ onInput address FirstName ] []
        ]
    , div [class "last" ]
        [ text "Last Name:"
        , input [ onInput address LastName  ] []
        ]
    , div [class "reference"]
        [ text (makeReference model)
        ]
    ]

makeReference model =
  let
    date = Date.fromTime model.time
    name = case (length model.firstName, length model.lastName) of
        (0,0) -> "Someone"
        (_,0) -> model.firstName
        (0,_) -> model.lastName
        _     -> model.firstName ++ " " ++ model.lastName
  in
    dateToString date ++ ": " ++ name ++ " likes to eat cheese"

--  Update
-- =========
--
-- These are the actions that we can receive, and how we alter the
-- model in repsonse to these actions coming in.
--
type Action = FirstName String
            | LastName String

update : (Time, Action) -> Model -> Model
update (timestamp,action) model =
  let
    model' = { model | time = timestamp }
  in
    case action of
        FirstName str -> { model' | firstName = str }
        LastName str  -> { model' | lastName = str }

--  Helpers
-- =========
--
-- Helper methods..
--
-- Pretty print a date as a string:
--

dateToString : Date -> String
dateToString d = day d ++ ", " ++ month d ++ " " ++ dayOfMonth d ++ daySuffix d

month : Date -> String
month d = case Date.month d of
    Jan -> "January"
    Feb -> "Febuary"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"

day : Date -> String
day d = case Date.dayOfWeek d of
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"

year : Date -> String
year d = toString (Date.year d)

dayOfMonth : Date -> String
dayOfMonth d = toString (Date.day d)

daySuffix d =
  let n = Date.day d
  in if n == 1 || n == 21 || n == 31 then "st"
     else if n == 2 || n == 22 then "nd"
     else if n == 3 || n == 23 then "rd"
     else "th"

--
-- Get text from an input when it changes:
--
onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (contentToValue >> Signal.message address)
