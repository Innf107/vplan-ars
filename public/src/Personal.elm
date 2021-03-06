port module Personal exposing (..)

import Browser
import Html exposing (..)
import Html.Events as E exposing (onInput)
import Html.Attributes as A
import Html.Lazy as L
import Http
import VPlanTypes exposing (..)
import Status as S exposing (Status(..))
import Json.Encode as JE
import Json.Decode as JD

port saveToStorage : JE.Value -> Cmd msg

main = Browser.document {init=init, subscriptions=subs, update=update, view=view}

type alias Model = {
                    online: Bool,
                    panic: Maybe String,
                    storage: List StorageItem,
                    vplan: Status VPlan,
                    kuerzel: Status (List StorageItem),
                    selectedDay: Int,
                    visibleKlassen: List UntisKlasse
                    }

init : JD.Value -> (Model, Cmd Msg)
init storageItems = (let model = {online=True,
                                  panic=Nothing,
                                  storage=[],
                                  vplan=Loading,
                                  kuerzel=Loading,
                                  selectedDay=0,
                                  visibleKlassen=[]
                                  }
                        in
                        getStorage model storageItems,
                         Cmd.batch [
                            Http.get {url="/json", expect=Http.expectJson ReceivedData uDataDecoder},
                            Http.get {url="/json/kuerzel", expect=Http.expectJson ReceivedKuerzel decodeStorage},
                            Http.get {url="/ping", expect=Http.expectString ReceivedPing}
                        ])

getStorage : Model -> JD.Value -> Model
getStorage model storageItems = case JD.decodeValue decodeStorage storageItems of
                                Err e -> {model|panic=Just <| JD.errorToString e}
                                Ok x -> {model|storage=x}

subs : Model -> Sub Msg
subs model = Sub.none

type Msg = NOP
         | ReceivedPing (Result Http.Error String)
         | ReceivedData (Result Http.Error UntisData)
         | ReceivedKuerzel (Result Http.Error (List StorageItem))
         | UpdateSelectedDay Int
         | UpdateStorage (List StorageItem)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    NOP -> (model, Cmd.none)
    ReceivedPing res -> case res of
        Err _ -> ({model|online=False}, Cmd.none)
        Ok s -> case s of
            "\"true\"" -> ({model|online=True}, Cmd.none)
            _ -> ({model|online=False}, Cmd.none)

    ReceivedData res -> case res of
        Ok udata -> ({model|vplan=Loaded udata.vplan}, Cmd.none)
        Err e -> ({model|vplan=Error (errToStr e)}, Cmd.none)
    ReceivedKuerzel res -> ({model|kuerzel=res |> S.fromResult errToStr}, Cmd.none)
    UpdateSelectedDay d -> ({model|selectedDay=d}, Cmd.none)
    UpdateStorage st -> ({model|storage=st}, saveToStorage <| encodeStorage st)

view : Model -> Browser.Document Msg
view model = {
        title="VPlan Personal",
        body=
        viewNavbar 2 ++
        case model.panic of
            Just p -> viewPanic p
            Nothing -> case model.vplan of
                Loading  -> viewLoading
                Error e  -> viewPanic e
                Loaded v -> viewLoaded model v
    }

viewLoading : List (Html Msg)
viewLoading = [
        h1 [] [text "Loading..."]
    ]

viewPanic : String -> List (Html Msg)
viewPanic p = [
        h1 [] [pre [] [text <| "Unrecoverable Error: " ++ p]]
    ]

viewLoaded : Model -> VPlan -> List (Html Msg)
viewLoaded model vplan = [
        --a [A.class "backButton", A.href "/"] [text "zurück"],
        (if not model.online then h3 [A.class "alert"] [text "Offline! Das ist der letzte Stand des Vertretungsplans."]
        else text "")]
        ++
        case (getAt model.selectedDay vplan) of
            Just x  -> viewDay model vplan x
            Nothing -> [h1 [] [pre [] [text "Error! Day doesn't exist!"]]]


viewDay : Model -> VPlan -> UntisDay -> List (Html Msg)
viewDay model vplan day = let dayAmount = List.length vplan in
    [
        div [A.class "headers"] [
            h1 [] [text "Vertretungsplan ARS"],
            L.lazy2 (\dayA selDay -> h3 [A.class "amountHeader"] [text <| (String.fromInt <| selDay + 1) ++ " / " ++ String.fromInt dayA]) dayAmount model.selectedDay,
            L.lazy (\day_ -> h3 [A.class "dateHeader"] [text day_]) day.day
        ],
        L.lazy (\() -> button [A.class "bleft",  E.onClick <| UpdateSelectedDay ((model.selectedDay - 1) |> modBy dayAmount)] [text "<—"]) (),
        L.lazy (\() -> button [A.class "bright", E.onClick <| UpdateSelectedDay ((model.selectedDay + 1) |> modBy dayAmount)] [text "—>"]) (),
        button [A.classList [("resolveKuerzel", True), ("active", sGet "resolveKuerzel" model.storage |> Maybe.map boolFromString |> Maybe.withDefault False)], onClick (UpdateStorage (sSave {key="resolveKuerzel", value=strFromBool <| Maybe.withDefault False <| Maybe.map not <| Maybe.map boolFromString <| sGet "resolveKuerzel" model.storage} model.storage))] [text "Kürzel auflösen"],


        div [A.class "inputContainer"] [
            p [] [text "Klasse"],
            input [A.id "klasse", A.placeholder "Klasse", A.value (model.storage |> sGet "klasse" |> Maybe.withDefault ""),
                   onInput (\s -> UpdateStorage <| sSave {key="klasse", value=s} model.storage)] []
        ],
        case getVisible day.klassen model.storage of
            [] -> h1 [] [text "Keine Ergebnisse"]
            ks -> table [] <| List.concatMap (\ k -> viewKlasse k (if model.storage |> sGetBool "resolveKuerzel" then model.kuerzel |> S.withDefault [] else [])) ks
    ]

getVisible : List UntisKlasse -> List StorageItem -> List UntisKlasse
getVisible klassen storage = storage |> sGet "klasse" |> Maybe.map (\k -> List.filter (\x -> String.contains k x.name) klassen) |> Maybe.withDefault klassen

