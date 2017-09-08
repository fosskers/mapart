module Art exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

---

type Colour = GreenRed | Spectrum | BlueGreen | PurpleYellow | BrownBlue

type Event = Choice Colour

type alias State = { colour : Colour }

main = Html.program { init = (State GreenRed, Cmd.none)
                    , view = view
                    , update = update
                    , subscriptions = \_ -> Sub.none
                    }

update : Event -> State -> (State, Cmd Event)
update event _ =
    case event of
        Choice c -> (State c, Cmd.none)

-- | Generate a clickable button based on a `Colour`.
toButton : Colour -> Html Event
toButton c = button [onClick <| Choice c] [text <| toString c]

view : State -> Html Event
view s =
    div []
        [ img [src <| "colour/" ++ String.toLower (toString s.colour)] []
        , div [] <| List.map toButton [GreenRed, Spectrum, BlueGreen, PurpleYellow, BrownBlue]
        ]
