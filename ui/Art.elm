module Art exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

---

type Colour = GreenRed | Spectrum | BlueGreen | PurpleYellow | BrownBlue

type Event = Choice Colour | Nav Navbar.State

type alias State = { colour : Colour
                   , navbar : Navbar.State }

main = Html.program { init = let (nbs, cmd) = Navbar.initialState Nav in (State GreenRed nbs, cmd)
                    , view = view
                    , update = update
                    , subscriptions = \s -> Navbar.subscriptions s.navbar Nav }

update : Event -> State -> (State, Cmd Event)
update event state =
    case event of
        Choice c -> ({ state | colour = c }, Cmd.none)
        Nav nbs -> ({ state | navbar = nbs }, Cmd.none)

-- | Generate a clickable button based on a `Colour`.
toButton : State -> Colour -> Html Event
toButton state c =
    let b = if state.colour == c then Button.outlineSuccess else Button.outlineSecondary
    in Button.button [b, Button.block, Button.attrs [onClick <| Choice c]] [text <| toString c]

-- | The appearance and behaviour of the top-bar.
navbar : State -> Html Event
navbar state = Navbar.config Nav
             |> Navbar.withAnimation
             |> Navbar.info
             |> Navbar.fixTop
             |> Navbar.brand [ href "#" ]
                [ img [ src "assets/azavea-logo.png"
                      , class "d-inline-block align-top"
                      , style [ ("width", "30px") ] ] []
                , text " Map Art" ]
             |> Navbar.items
                [ Navbar.itemLink [href "#"] [ text "About" ]
                , Navbar.itemLink [href "#"] [ text "Purchasing" ]]
             -- |> Navbar.customItems
             --    [ Navbar.formItem [] [Input.text [ Input.placeholder "Filter by location..." ]]]
             |> Navbar.view state.navbar

view : State -> Html Event
view state =
    div []
        [ navbar state
        , Grid.container [ style [ ("padding-top", "5%") ] ]
            [ Grid.row [ Row.centerXs ]
                  [ Grid.col [ Col.xsAuto ]
                        [ img [src <| "colour/" ++ String.toLower (toString state.colour)] [] ]]
            , Grid.row [ Row.centerXs, Row.attrs [ style [("padding-top", "3%")]] ]
                <| List.map (\c -> Grid.col [ Col.xs2 ] [ toButton state c ])
                    [GreenRed, Spectrum, BlueGreen, PurpleYellow, BrownBlue]
            ]
        ]
