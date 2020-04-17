module Teacher exposing (..)

import Browser
import Html exposing (..)
import Html.Events as E exposing (onInput)
import Html.Attributes as A
import Html.Lazy as L
import Http
import VPlanTypes exposing (..)
import Status as S exposing (Status(..))


main = Browser.document {init=init, subscriptions=subs, update=update, view=view}

type alias Model = {
        online: Bool,
        vplan: Status VPlan,
        kuerzel: String,
        selectedDay: Int,
        klassenWithTeacher: List UntisKlasse
    }

init : () -> (Model, Cmd Msg)
init _ = ({
        online=True,
        vplan=Loading,
        kuerzel="",
        selectedDay=0,
        klassenWithTeacher=[]
    }, Cmd.batch[
        Http.get {
            url="/json",
            expect=Http.expectJson ReceivedUData uDataDecoder
        },
        Http.get {
            url="/ping",
            expect=Http.expectString ReceivedPing
        }
    ])

subs : Model -> Sub Msg
subs model = Sub.none

type Msg = NOP
         | ReceivedPing (Result Http.Error String)
         | ReceivedUData (Result Http.Error UntisData)
         | UpdateKuerzel String
         | UpdateSelectedDay Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    NOP -> (model, Cmd.none)
    ReceivedPing res -> case res of
        Err _ -> ({model|online=False}, Cmd.none)
        Ok s -> case s of
            "\"true\"" -> ({model|online=True}, Cmd.none)
            _ -> ({model|online=False}, Cmd.none)
    ReceivedUData res -> ({model|vplan=S.map .vplan <| S.fromResult errToStr res}, Cmd.none)
    UpdateKuerzel str -> ({model|kuerzel=String.toUpper <| String.trim str,
                         klassenWithTeacher=S.map (\v -> getAt model.selectedDay v) model.vplan
                         |> S.toMaybe
                         |> Maybe.andThen identity
                         |> Maybe.map (\d -> getHoursWithTeacher d <| String.toUpper <| String.trim str)
                         |> Maybe.withDefault []},Cmd.none)
    UpdateSelectedDay d -> update (UpdateKuerzel model.kuerzel) {model|selectedDay=d}


view : Model -> Browser.Document Msg
view model = {
        title="VPlan Lehrer",
        body=
            viewNavbar 1 ++
            (case model.vplan of
                Loading  -> viewLoading model
                Error e  -> viewError model e
                Loaded v -> viewLoaded model v
            )
    }

viewLoading : Model -> List (Html Msg)
viewLoading model = [
        h1 [] [text "Vertretungsplan Lehrer"],
        h1 [] [text "Loading..."]
    ]

viewError : Model -> String -> List (Html Msg)
viewError model error = [
        h1 [] [pre [] [text <| "Error: " ++ error]]
    ]

viewLoaded : Model -> VPlan -> List (Html Msg)
viewLoaded model vplan = [
        --a [A.class "backButton", A.href "/"] [text "zurück"],
        (if not model.online then h3 [A.class "alert"] [text "Offline! Das ist der letzte Stand des Vertretungsplans."]
        else text ""),
        h1 [] [text "Vertretungsplan Lehrer"]]
        ++
        case (getAt model.selectedDay vplan) of
            Just x  -> viewDay model vplan x
            Nothing -> [h1 [] [pre [] [text "Error! Day doesn't exist!"]]]

viewDay : Model -> VPlan -> UntisDay -> List (Html Msg)
viewDay model vplan day = let dayAmount = List.length vplan in
    [
        L.lazy2 (\dayA selDay -> h3 [A.class "amountHeader"] [text <| (String.fromInt <| selDay + 1) ++ " / " ++ String.fromInt dayA]) dayAmount model.selectedDay,
        L.lazy (\day_ -> h3 [A.class "dateHeader"] [text day_]) day.day,
        L.lazy (\() -> button [A.class "bleft",  E.onClick <| UpdateSelectedDay ((model.selectedDay - 1) |> modBy dayAmount)] [text "<—"]) (),
        L.lazy (\() -> button [A.class "bright", E.onClick <| UpdateSelectedDay ((model.selectedDay + 1) |> modBy dayAmount)] [text "—>"]) (),
        input [A.placeholder "Lehrerkürzel", A.class "inputTeacher", onInput UpdateKuerzel] [],

        case model.kuerzel of
            "" -> text ""
            _  -> case model.klassenWithTeacher of
                [] -> h1 [] [text "Keine Ergebnisse"]
                --TODO: Add kuerzel
                _  -> table [] <| List.concatMap (\k -> viewKlasse k []) model.klassenWithTeacher
    ]
