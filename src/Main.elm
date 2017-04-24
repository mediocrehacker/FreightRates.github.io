module Main exposing (..)

import Autocomplete
import Html exposing (Html, program, div, text, h1, h2, h3, span, p, ul, li, img)
import Html.Attributes exposing (style, class, src)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Material
import Material.Button as Button
import Material.Chip as Chip
import Material.Color as Color
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Progress as Loading
import Material.Scheme as Scheme
import Material.Toggles as Toggles
import Material.Typography as Typo
import Material.Tabs as Tabs
import RemoteData exposing (RemoteData(..))
import RemoteData exposing (RemoteData(..), WebData)
import SearchSeaPort exposing (..)
import Types exposing (SeaPort, Tariff, tariffs)


-- Model


type alias Model =
    { mdl : Material.Model
    , pol : SearchSeaPort.Model
    , pod : SearchSeaPort.Model
    , currentFocus : Focused
    , tariffs : WebData (List Tariff)
    , errors : List String
    , tab : Int
    }


type Focused
    = Pol
    | Pod
    | None


searchSeaPortInit : SearchSeaPort.Model
searchSeaPortInit =
    SearchSeaPort.init


init : ( Model, Cmd Msg )
init =
    { mdl = Material.model
    , pol =
        { searchSeaPortInit
            | label = "Port of Loading"
            , selectedSeaPort = Just (SeaPort "RUVVO" "Vladivostok" "Russis")
        }
    , pod =
        { searchSeaPortInit
            | label = "Port of Discharge"
            , selectedSeaPort = Just (SeaPort "CNSHA" "Shanghai" "China")
        }
    , currentFocus = None
    , tariffs = Success tariffs
    , errors = []
    , tab = -1
    }
        ! []



-- Update


type Msg
    = Mdl (Material.Msg Msg)
    | SearchPol SearchSeaPort.Msg
    | SearchPod SearchSeaPort.Msg
    | ChildMsg ChildPortalMsg
    | GetTariffs
    | NewResponse (WebData (List Tariff))
    | SelectTab Int
    | NoOp


type ChildPortalMsg
    = SearchSeaPolMsg SearchSeaPort.Msg
    | SearchSeaPodMsg SearchSeaPort.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTariffs ->
            let
                pol =
                    model.pol.selectedSeaPort

                pod =
                    model.pod.selectedSeaPort
            in
                case ( pol, pod ) of
                    ( Just seaPort1, Just seaPort2 ) ->
                        ( model, getTariffs seaPort1 seaPort2 )

                    _ ->
                        ( model, Cmd.none )

        NewResponse response ->
            ( { model | tariffs = response }, Cmd.none )

        ChildMsg subMsg ->
            case subMsg of
                SearchSeaPolMsg spMsg ->
                    let
                        ( spModel, spCmd ) =
                            SearchSeaPort.update spMsg model.pol
                    in
                        ( { model | pol = spModel }
                        , Cmd.map (\b -> (ChildMsg (SearchSeaPolMsg b))) spCmd
                        )

                SearchSeaPodMsg spMsg ->
                    let
                        ( spModel, spCmd ) =
                            SearchSeaPort.update spMsg model.pod
                    in
                        ( { model | pod = spModel }
                        , Cmd.map (\b -> (ChildMsg (SearchSeaPodMsg b))) spCmd
                        )

        SearchPol autoMsg ->
            let
                ( pol, cmd_ ) =
                    (SearchSeaPort.update autoMsg model.pol)
            in
                ( { model
                    | pol = pol
                    , currentFocus = Pol
                  }
                , Cmd.map (\b -> ChildMsg (SearchSeaPolMsg b)) cmd_
                )

        SearchPod autoMsg ->
            let
                ( pod, cmd_ ) =
                    (SearchSeaPort.update autoMsg model.pod)
            in
                ( { model
                    | pod = pod
                    , currentFocus = Pod
                  }
                , Cmd.map (\b -> ChildMsg (SearchSeaPodMsg b)) cmd_
                )

        SelectTab id ->
            { model | tab = id } ! []

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
            [ text "Freight Rates" ]
        , Options.styled h3
            [ Typo.headline ]
            [ text "Search Best Freight Shipping Rates" ]
        , viewBody model
        ]


viewBody : Model -> Html Msg
viewBody model =
    div []
        [ Grid.grid
            []
            [ Grid.cell [ Grid.size Grid.All 5, Grid.size Grid.Tablet 12 ]
                [ viewPol model.pol ]
            , Grid.cell [ Grid.size Grid.All 5, Grid.size Grid.Tablet 12 ]
                [ viewPod model.pod ]
            , Grid.cell [ Grid.size Grid.All 2, Grid.size Grid.Tablet 12 ]
                [ div
                    [ style [ ( "padding", "10px 0" ) ] ]
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Options.onClick GetTariffs
                        ]
                        [ text "Raised button" ]
                    ]
                ]
            ]
        , viewTariffs model
        ]


viewTariffs : Model -> Html Msg
viewTariffs model =
    case model.tariffs of
        NotAsked ->
            text "Initialising."

        Loading ->
            text "Loading."

        Failure err ->
            text ("Error: " ++ toString err)

        Success tariffs ->
            Grid.grid
                []
                ([ Grid.cell
                    [ Grid.hide Grid.Phone
                    , Grid.size Grid.All 12
                    ]
                    [ filtersView model ]
                 ]
                    ++ viewSuccessTariffs tariffs
                )


filtersView : Model -> Html Msg
filtersView model =
    Tabs.render Mdl
        [ 0 ]
        model.mdl
        [ Tabs.ripple
        , Tabs.onSelectTab SelectTab
        , Tabs.activeTab model.tab
        ]
        [ Tabs.label
            [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ Options.css "width" "4px" ] []
            , text "Container Type"
            ]
        , Tabs.label
            [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ Options.css "width" "4px" ] []
            , text "Price range"
            ]
        , Tabs.label
            [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ Options.css "width" "4px" ] []
            , text "Company name"
            ]
        , Tabs.label
            [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ Options.css "width" "4px" ] []
            , text "More filters"
            ]
        ]
        [ case model.tab of
            0 ->
                containersTab

            1 ->
                priceTab

            2 ->
                companiesTab

            _ ->
                companiesTab
        ]


containersTab : Html Msg
containersTab =
    p [] [ text "Container Type" ]


priceTab : Html Msg
priceTab =
    p [] [ text "Price Range" ]


companiesTab : Html Msg
companiesTab =
    p [] [ text "Company Name" ]


viewSuccessTariffs : List Tariff -> List (Grid.Cell Msg)
viewSuccessTariffs tariffs =
    List.filterMap
        (\x -> viewTariff x)
        tariffs


viewTariff : Tariff -> Maybe (Grid.Cell Msg)
viewTariff t =
    let
        price =
            "$"
                ++ (toString
                        ((Result.withDefault 0 (String.toFloat t.baf))
                            + (Result.withDefault 0 (String.toFloat t.freight))
                        )
                   )
    in
        if t.container == "" then
            Nothing
        else
            Just <|
                Grid.cell
                    [ Grid.size Grid.All 12
                    , Elevation.e2
                    ]
                    [ Grid.grid
                        []
                        [ Grid.cell
                            [ Grid.size Grid.All 3
                            , Options.css "min-height" "150px"
                            ]
                            [ Chip.span []
                                [ Chip.content []
                                    [ text (t.owners) ]
                                ]
                            , Chip.span [ Options.css "margin-left" "10px" ]
                                [ Chip.content []
                                    [ text (t.status) ]
                                ]
                            , img
                                [ src ("static/img/" ++ (String.toLower t.company) ++ ".png")
                                , style [ ( "margin-top", "30px" ) ]
                                ]
                                []
                            ]
                        , Grid.cell
                            [ Grid.size Grid.All 2
                            , Grid.align Grid.Middle
                            , Options.css "text-align" "center"
                            ]
                            [ Options.styled p
                                [ Typo.title ]
                                [ text "Vladivostok" ]
                            , Options.styled p
                                [ Typo.subhead ]
                                [ text t.pol ]
                            ]
                        , Grid.cell
                            [ Grid.size Grid.All 3
                            , Grid.align Grid.Middle
                            ]
                            [ div [ style [ ( "padding-top", "0px" ), ( "text-align", "center" ) ] ]
                                [ p [] [ text t.container ]
                                ]
                            , Loading.progress 100
                            , div [ style [ ( "padding-top", "10px" ), ( "text-align", "center" ) ] ] [ Icon.view "directions_boat" [ Icon.size24, Color.text Color.primary ] ]
                            ]
                        , Grid.cell
                            [ Grid.size Grid.All 2
                            , Grid.align Grid.Middle
                            , Options.css "text-align" "center"
                            ]
                            [ Options.styled p
                                [ Typo.title ]
                                [ text "Shanghai" ]
                            , Options.styled p
                                [ Typo.subhead ]
                                [ text t.pod ]
                            ]
                        , Grid.cell
                            [ Grid.size Grid.All 2
                            , Options.css "text-align" "center"
                            , Grid.align Grid.Middle
                            ]
                            [ Options.styled p
                                [ Typo.title ]
                                [ text price ]
                            ]
                        ]
                    ]


viewPol : SearchSeaPort.Model -> Html Msg
viewPol pol =
    Html.map SearchPol (SearchSeaPort.view pol)


viewPod : SearchSeaPort.Model -> Html Msg
viewPod pod =
    Html.map SearchPod (SearchSeaPort.view pod)


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



-- HTTP


getTariffs : SeaPort -> SeaPort -> Cmd Msg
getTariffs pol pod =
    Http.get ("http://seaports.herokuapp.com/api/v1/tariffs/pols/" ++ pol.code ++ "/pods/" ++ pod.code) decodeTariffs
        |> RemoteData.sendRequest
        |> Cmd.map NewResponse


decodeTariffs : Decode.Decoder (List Tariff)
decodeTariffs =
    (Decode.field "tariffs" (Decode.list decodeTariff))


decodeTariff : Decode.Decoder Tariff
decodeTariff =
    decode Tariff
        |> required "company" Decode.string
        |> required "pol" Decode.string
        |> required "pod" Decode.string
        |> required "container" Decode.string
        |> required "status" Decode.string
        |> required "owners" Decode.string
        |> required "freight" Decode.string
        |> required "baf" Decode.string
