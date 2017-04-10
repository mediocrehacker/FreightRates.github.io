module Main exposing (..)

import SearchSeaPort exposing (..)
import Html exposing (Html, program, div, text, h1, h2, h3, span, p)
import Html.Attributes exposing (style, class)
import Autocomplete
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
    , pol : SearchSeaPort.Model
    , pod : SearchSeaPort.Model
    , currentFocus : Focused
    }


type Focused
    = Pol
    | Pod
    | None


init : ( Model, Cmd Msg )
init =
    ( { mdl = Material.model
      , pol = SearchSeaPort.init
      , pod = SearchSeaPort.init
      , currentFocus = None
      }
    , Cmd.none
    )



-- Update


type Msg
    = Mdl (Material.Msg Msg)
    | SearchPol SearchSeaPort.Msg
    | SearchPod SearchSeaPort.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchPol autoMsg ->
            ( { model
                | pol = Tuple.first <| SearchSeaPort.update autoMsg model.pol
                , currentFocus = Pol
              }
            , Cmd.none
            )

        SearchPod autoMsg ->
            ( { model
                | pod = Tuple.first <| SearchSeaPort.update autoMsg model.pod
                , currentFocus = Pod
              }
            , Cmd.none
            )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        NoOp ->
            ( model, Cmd.none )



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
        , viewPol model.pol
        , viewPod model.pod
        ]


viewPol : SearchSeaPort.Model -> Html Msg
viewPol pol =
    div [ class "example" ]
        [ div [ class "example-info" ]
            [ p [] [ text "Pol" ]
            ]
        , div [ class "example-autocomplete" ]
            [ Html.map SearchPol (SearchSeaPort.view pol)
            ]
        ]


viewPod : SearchSeaPort.Model -> Html Msg
viewPod pod =
    div [ class "example" ]
        [ div [ class "example-info" ]
            [ p [] [ text "Pol" ]
            ]
        , div [ class "example-autocomplete" ]
            [ Html.map SearchPod (SearchSeaPort.view pod)
            ]
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
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentFocus of
        Pol ->
            Sub.map SearchPol (SearchSeaPort.subscriptions model.pol)

        Pod ->
            Sub.map SearchPod (SearchSeaPort.subscriptions model.pod)

        None ->
            Sub.none
