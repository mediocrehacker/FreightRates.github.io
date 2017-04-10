module SearchSeaPort exposing (..)

import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Decode as Json
import Json.Encode as JE
import Dom
import Task


main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription


type alias Model =
    { seaPorts : List SeaPort
    , autoState : Autocomplete.State
    , howManyToShow : Int
    , query : String
    , selectedSeaPort : Maybe SeaPort
    , showMenu : Bool
    }


init : Model
init =
    { seaPorts = presidents
    , autoState = Autocomplete.empty
    , howManyToShow = 5
    , query = ""
    , selectedSeaPort = Nothing
    , showMenu = False
    }


type Msg
    = SetQuery String
    | SetAutoState Autocomplete.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectSeaPortKeyboard String
    | SelectSeaPortMouse String
    | PreviewSeaPort String
    | OnFocus
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| (acceptableSeaPorts newQuery model.seaPorts)
            in
                { model | query = newQuery, showMenu = showMenu, selectedSeaPort = Nothing } ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState (acceptableSeaPorts model.query model.seaPorts)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (acceptableSeaPorts model.query model.seaPorts)

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        { model | query = "" }
                            |> removeSelection
                            |> resetMenu

                escapedModel =
                    case model.selectedSeaPort of
                        Just seaPort ->
                            if model.query == seaPort.name then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
                escapedModel ! []

        Wrap toTop ->
            case model.selectedSeaPort of
                Just seaPort ->
                    update Reset model

                Nothing ->
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem updateConfig (acceptableSeaPorts model.query model.seaPorts) model.howManyToShow model.autoState
                            , selectedSeaPort = List.head <| List.reverse <| List.take model.howManyToShow <| (acceptableSeaPorts model.query model.seaPorts)
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (acceptableSeaPorts model.query model.seaPorts) model.howManyToShow model.autoState
                            , selectedSeaPort = List.head <| List.take model.howManyToShow <| (acceptableSeaPorts model.query model.seaPorts)
                        }
                            ! []

        Reset ->
            { model | autoState = Autocomplete.reset updateConfig model.autoState, selectedSeaPort = Nothing } ! []

        SelectSeaPortKeyboard id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
                newModel ! []

        SelectSeaPortMouse id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
                ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "president-input") )

        PreviewSeaPort id ->
            { model | selectedSeaPort = Just <| getSeaPortAtId model.seaPorts id } ! []

        OnFocus ->
            model ! []

        NoOp ->
            model ! []


resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu


removeSelection model =
    { model | selectedSeaPort = Nothing }


getSeaPortAtId seaPorts id =
    List.filter (\seaPort -> seaPort.name == id) seaPorts
        |> List.head
        |> Maybe.withDefault (SeaPort "" "" "")


setQuery model id =
    { model
        | query = .name <| getSeaPortAtId model.seaPorts id
        , selectedSeaPort = Just <| getSeaPortAtId model.seaPorts id
    }


resetMenu model =
    { model
        | autoState = Autocomplete.empty
        , showMenu = False
    }


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.andThen
                    fromResult

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason

        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []

        query =
            case model.selectedSeaPort of
                Just seaPort ->
                    seaPort.name

                Nothing ->
                    model.query

        activeDescendant attributes =
            case model.selectedSeaPort of
                Just seaPort ->
                    (attribute "aria-activedescendant"
                        seaPort.name
                    )
                        :: attributes

                Nothing ->
                    attributes
    in
        div []
            (List.append
                [ input
                    (activeDescendant
                        [ onInput SetQuery
                        , onFocus OnFocus
                        , onWithOptions "keydown" options dec
                        , value query
                        , id "president-input"
                        , class "autocomplete-input"
                        , autocomplete False
                        , attribute "aria-owns" "list-of-presidents"
                        , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                        , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                        , attribute "role" "combobox"
                        , attribute "aria-autocomplete" "list"
                        ]
                    )
                    []
                ]
                menu
            )


acceptableSeaPorts : String -> List SeaPort -> List SeaPort
acceptableSeaPorts query seaPorts =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .name) seaPorts


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableSeaPorts model.query model.seaPorts)) ]


updateConfig : Autocomplete.UpdateConfig Msg SeaPort
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewSeaPort maybeId
                else if code == 13 then
                    Maybe.map SelectSeaPortKeyboard maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewSeaPort id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectSeaPortMouse id
        , separateSelections = False
        }


viewConfig : Autocomplete.ViewConfig SeaPort
viewConfig =
    let
        customizedLi keySelected mouseSelected seaPort =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id seaPort.name
                ]
            , children = [ Html.text seaPort.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }



-- Ports


type alias SeaPort =
    { code : String
    , name : String
    , country : String
    }


presidents : List SeaPort
presidents =
    [ SeaPort "RUVVO" "Vladivostok" "Russis"
    , SeaPort "CNSHA" "Shanghai" "China"
    , SeaPort "HKHKG" "Hong Kong" "Hong Kong"
    , SeaPort "CNSWA" "Shantou" "China"
    , SeaPort "CNDLC" "Dalian" "China"
    ]
