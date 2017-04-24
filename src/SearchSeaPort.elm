module SearchSeaPort exposing (..)

import Types exposing (SeaPort)
import RemoteData exposing (RemoteData(..))
import Http
import Json.Decode as Decode
import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Decode as Json
import Json.Encode as JE
import Dom
import Task
import Material
import Material.Textfield as Textfield
import Material.Options as Options


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
    { remoteSeaPorts : RemoteData Http.Error (List SeaPort)
    , label : String
    , seaPorts : List SeaPort
    , autoState : Autocomplete.State
    , howManyToShow : Int
    , query : String
    , selectedSeaPort : Maybe SeaPort
    , showMenu : Bool
    , mdl : Material.Model
    }


initLabel : String -> Model
initLabel label =
    { init | label = label }


init : Model
init =
    { remoteSeaPorts = NotAsked
    , label = ""
    , seaPorts = seaPorts
    , autoState = Autocomplete.empty
    , howManyToShow = 5
    , query = ""
    , selectedSeaPort = Nothing
    , showMenu = False
    , mdl = Material.model
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
    | NewSeaPorts (Result Http.Error (List SeaPort))
    | ReceiveSeaPorts (RemoteData.WebData (List SeaPort))
    | Mdl (Material.Msg Msg)
    | Batch (List Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Batch listOfMsg ->
            let
                ( finalModel, listOfFx ) =
                    List.foldl
                        (\msg ->
                            \( mdl, fxList ) ->
                                let
                                    ( newModel, newFx ) =
                                        update msg mdl
                                in
                                    ( newModel, fxList ++ [ newFx ] )
                        )
                        ( model, [] )
                        listOfMsg
            in
                ( finalModel, Cmd.batch listOfFx )

        NewSeaPorts (Ok seaPorts) ->
            let
                _ =
                    Debug.log "msg:" seaPorts
            in
                ( { model | seaPorts = seaPorts }, Cmd.none )

        NewSeaPorts (Err err) ->
            let
                _ =
                    Debug.log "msg:" err
            in
                ( model, Cmd.none )

        ReceiveSeaPorts resp ->
            case resp of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure e ->
                    ( model, Cmd.none )

                Success seaPorts ->
                    ( { model | seaPorts = seaPorts }
                    , Cmd.none
                    )

        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| (acceptableSeaPorts newQuery model.seaPorts)
            in
                ( { model | query = newQuery, showMenu = showMenu, selectedSeaPort = Nothing }
                , Debug.log "SetQuery: " (getSeaPorts newQuery)
                )

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
                ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "seaport-input") )

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
                [ Textfield.render Mdl
                    [ 0 ]
                    model.mdl
                    -- (activeDescendant
                    [ Options.onInput SetQuery
                    , Options.onFocus OnFocus
                    , Options.onWithOptions "keydown" options dec
                    , Options.css "width" "100%"
                    , Textfield.value query
                    , Textfield.label model.label
                    , Textfield.floatingLabel
                    , Options.dispatch Batch
                    , Options.attribute <| autocomplete False
                    , Options.attribute <| attribute "aria-owns" "list-of-seaports"
                    , Options.attribute <| attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                    , Options.attribute <| attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                    , Options.attribute <| attribute "role" "combobox"
                    , Options.attribute <| attribute "aria-autocomplete" "list"
                    ]
                    -- )
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
        -- List.filter (String.contains lowerQuery << String.toLower << .name) seaPorts
        seaPorts



-- HTTP


getSeaPorts : String -> Cmd Msg
getSeaPorts query =
    Http.send NewSeaPorts
        (Http.get
            ("https://seaports.herokuapp.com/seaports.json?q=" ++ query)
            decodeSeaPorts
        )


decodeSeaPorts : Decode.Decoder (List SeaPort)
decodeSeaPorts =
    (Decode.field "seaports" (Decode.list decodeSeaPort))


decodeSeaPort : Decode.Decoder SeaPort
decodeSeaPort =
    Decode.map3 SeaPort
        (Decode.field "code" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "country" Decode.string)



-- getSeaPorts : Cmd (RemoteData.WebData (List SeaPort))
-- getSeaPorts =
--     Http.get
--         "http://seaports.herokuapp.com/seaports"
--         (Decode.field "seaports" Decode.string)
--         |> RemoteData.sendRequest


viewMenu : Model -> Html Msg
viewMenu model =
    div [ style [ ( "position", "relative" ) ] ]
        [ div
            [ class "autocomplete-menu"
            , style
                [ ( "position", "absolute" )
                , ( "margin-top", "-15px" )
                , ( "background", "white" )
                , ( "color", "black" )
                , ( "border", "1px solid #DDD" )
                , ( "border-radius", "3px" )
                , ( "box-shadow", "0 0 5px rgba(0,0,0,0.1)" )
                , ( "width", "100%" )
                , ( "z-index", "10" )
                ]
            ]
            [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableSeaPorts model.query model.seaPorts)) ]
        ]


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
            let
                backgroundColor =
                    if keySelected || mouseSelected then
                        "#3366FF"
                    else
                        "#FFFFFF"
            in
                { attributes =
                    [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                    , id seaPort.name
                    , style
                        [ ( "display", "block" )
                        , ( "padding", "5px 10px" )
                        , ( "border-bottom", "1px solid #DDD" )
                        , ( "cursor", "pointer" )
                        , ( "background-color", backgroundColor )
                        ]
                    ]
                , children = [ Html.text (seaPortToString seaPort) ]
                }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul =
                [ style
                    [ ( "list-style", "none" )
                    , ( "padding", "0" )
                    , ( "margin", "auto" )
                    , ( "max-height", "200px" )
                    , ( "oveflow-y", "auto" )
                    ]
                ]
            , li = customizedLi
            }



-- Ports


seaPorts : List SeaPort
seaPorts =
    [ SeaPort "RUVVO" "Vladivostok" "Russis"
    , SeaPort "CNSHA" "Shanghai" "China"
    , SeaPort "HKHKG" "Hong Kong" "Hong Kong"
    , SeaPort "CNSWA" "Shantou" "China"
    , SeaPort "CNDLC" "Dalian" "China"
    ]


seaPortToString : SeaPort -> String
seaPortToString seaPort =
    seaPort.name ++ " , " ++ seaPort.country ++ " | " ++ seaPort.code
