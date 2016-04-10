import Html exposing (Html)
import Time exposing (timestamp, Time)
import Signal

import Reference

type Action = Noop | ReferenceAction Reference.Action

type alias Model = Reference.Model

--   Main
-- ========
--
-- wire everything together:
--
main : Signal Html
main = let myMailbox     = Signal.mailbox Noop
           modelSignal   = Signal.foldp update model (timestamp myMailbox.signal)
       in Signal.map (view myMailbox.address) modelSignal

--
-- Just use the reference components model:
--
model : Model
model = Reference.model

--
-- Forward to the reference components view:
--
view : Signal.Address Action -> Model -> Html
view address model =
    let referenceAddy = Signal.forwardTo address ReferenceAction
    in Reference.view referenceAddy model

--
-- We can have multiple components updating. Here we just
-- have the one, though.
--
update : (Time, Action) -> Reference.Model -> Reference.Model
update (time,action) model = case action of
    Noop -> model
    ReferenceAction refAction -> Reference.update (time,refAction) model