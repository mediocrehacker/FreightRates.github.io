module Main exposing (..)

import Html exposing (Html, program, div, text, h1, h2, h3, span, p)
import Html.Attributes exposing (style)
import Material
import Material.Scheme as Scheme
import Material.Color as Color
import Material.Layout as Layout
import Material.Toggles as Toggles
import Material.Button as Button
import Material.Options as Options
import Material.Typography as Typo
import Material.Icon as Icon
import Material.Grid as Grid
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Typography as Typography


-- Model


type alias Model =
    { mdl : Material.Model
    , pol : String
    , pod : String
    }


init : ( Model, Cmd Msg )
init =
    ( { mdl = Material.model
      , pol = ""
      , pod = ""
      }
    , Cmd.none
    )



-- Update


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- View


view : Model -> Html Msg
view model =
    Scheme.topWithScheme Color.Blue Color.Orange <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader ]
            { header = header
            , drawer = drawer
            , tabs = ( [], [] )
            , main = [ viewMain model ]
            }


viewMain : Model -> Html Msg
viewMain model =
    div
        [ style
            [ ( "padding-right", "8%" )
            , ( "padding-left", "8%" )
            ]
        ]
        [ Options.styled h1
            [ Color.text Color.primary ]
            [ text "FreightRates" ]
        , Options.styled h3
            [ Typo.headline ]
            [ text "Search Best Freight Shipping Rates" ]
        ]


header : List (Html Msg)
header =
    [ Layout.row []
        [ Layout.title [] [ text "FreightRates" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "#" ]
                [ span [] [ text "Sign In" ] ]
            ]
        ]
    ]


drawer : List (Html Msg)
drawer =
    [ Layout.title [] [ text "FreightRates" ]
    , Layout.navigation
        []
        [ Layout.link
            [ Layout.href "#" ]
            [ text "About" ]
        , Layout.link
            [ Layout.href "#" ]
            [ text "Benefits" ]
        ]
    ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\x -> Sub.none)
        }
