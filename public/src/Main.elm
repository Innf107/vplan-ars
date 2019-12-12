module Main exposing (main)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L
import VPlanTypes exposing (..)
import Browser
import Http
import Time
import Status as S exposing (Status(..))

main = Browser.document {init=init, subscriptions=subs, update=update, view=view}

type alias Model = {
        vplan: Status VPlan,
        loadingText: String,
        selectedDay: Int,
        expandedKlassen: List String,
        inFeedback: Bool,
        expandedMOTD: Bool,
        feedbackText: String
    }

init : () -> (Model, Cmd Msg)
init _ = ({
            vplan=Loading,
            loadingText="",
            selectedDay=0,
            expandedKlassen=[],
            inFeedback=False,
            expandedMOTD=False,
            feedbackText=""
          },
        Http.get {
            url="/json",
            expect=Http.expectJson LoadedVPlan vPlanDecoder
        })

type Msg = NOP
         | LoadedVPlan (Result (Http.Error) UntisData)
         | UpdateLoadingText
         | UpdateExpandedDays (List String)
         | UpdateSelectedDay Int
         | UpdateFeedbackText String
         | UpdateMOTDExpansion Bool
         | SelectFeedback Bool
         | SendFeedback


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    NOP -> (model, Cmd.none)
    UpdateLoadingText -> ({model|loadingText=String.left (String.length model.loadingText + 1 |> modBy 11) "Loading..."}, Cmd.none)
    UpdateExpandedDays newKlassen -> ({model|expandedKlassen=newKlassen}, Cmd.none)
    LoadedVPlan res -> ({model|vplan=S.map .vplan (S.fromResult errToStr res)}, Cmd.none)
    UpdateSelectedDay d -> ({model|selectedDay=d}, Cmd.none)
    UpdateFeedbackText t -> ({model|feedbackText=t}, Cmd.none)
    UpdateMOTDExpansion n -> ({model|expandedMOTD=n}, Cmd.none)
    SelectFeedback fb -> ({model|inFeedback=fb}, Cmd.none)
    SendFeedback -> ({model|inFeedback=False, feedbackText=""}, Http.request {
            method = "PUT",
            url="/feedback",
            headers = [],
            body = Http.stringBody "application/text" model.feedbackText,
            expect = Http.expectWhatever (\_ -> NOP),
            timeout = Nothing,
            tracker = Nothing
        })


subs : Model -> Sub Msg
subs model = case model.vplan of
                Loading -> Time.every 50 (\_ -> UpdateLoadingText)
                _ -> Sub.none

css : Html Msg
css = link [A.rel "stylesheet", A.href "main.css"] []

view : Model -> Browser.Document Msg
view model = {
            title="VPlan ARS",
            body=case model.vplan of
                Loading -> css::viewLoading model
                Error e -> css::viewError model e
                Loaded x-> css::viewLoaded model x
        }

viewLoading : Model -> List (Html Msg)
viewLoading model = [
        h1 [] [text model.loadingText]
    ]

viewError : Model -> String -> List (Html Msg)
viewError model error = [
        h1 [] [pre [] [text <| "Error getting VPlan: " ++ error]]
    ]


viewLoaded : Model -> VPlan -> List (Html Msg)
viewLoaded model vplan = [
        h1 [] [text "Vertretungsplan ARS"],
        case model.inFeedback of
            True  -> viewFeedback model
            False -> case getAt model.selectedDay vplan of
                        Nothing  -> pre [] [text "Error: Invalid day selected!"]
                        Just day -> viewDay model vplan day
    ]

viewDay : Model -> VPlan -> UntisDay -> Html Msg
viewDay model vplan day =
    let dayAmount = List.length vplan in
    div [] [
        a [A.class "backButton", A.href "/"] [text "zurück"],
        L.lazy2 (\dayA selDay -> h3 [A.class "amountHeader"] [text <| (String.fromInt <| selDay + 1) ++ " / " ++ String.fromInt dayA]) dayAmount model.selectedDay,
        L.lazy (\day_ -> h3 [A.class "dateHeader"] [text day_]) day.day,
        L.lazy (\() -> button [A.class "bleft",  onClick <| UpdateSelectedDay ((model.selectedDay - 1) |> modBy dayAmount)] [text "<—"]) (),
        L.lazy (\() -> button [A.class "bright", onClick <| UpdateSelectedDay ((model.selectedDay + 1) |> modBy dayAmount)] [text "—>"]) (),
        viewMOTD model day,
        table [] (day.klassen |> List.concatMap (\k -> if List.member k.name model.expandedKlassen then viewKlasseExpanded model k else viewKlasseCollapsed model k)),
        button [A.class "enterFeedback", onClick <| SelectFeedback True] [text "Feedback?"]
    ]

viewKlasseCollapsed : Model -> UntisKlasse -> List (Html Msg)
viewKlasseCollapsed model klasse = [
        tr [A.class "klasse collapsed"] [
            th [A.class "klasse collapsed", A.colspan 5] [
                button [A.class "klasse collapsed", onClick (UpdateExpandedDays (klasse.name::model.expandedKlassen))] [text <| "➤" ++ klasse.name]
            ]
        ]
    ]

viewKlasseExpanded : Model -> UntisKlasse -> List (Html Msg)
viewKlasseExpanded model klasse =
        tr [A.class "klasse expanded"] [
            th [A.class "klasse expanded", A.colspan 5] [
                button [A.class "klasse expanded", onClick (UpdateExpandedDays (delete klasse.name model.expandedKlassen))] [text <| "▼" ++ klasse.name]
            ]
        ]
        ::List.map (\hour ->
            tr [A.class "klasse expanded"] [
                    td [A.class "klasse expanded"] <| showStunde hour.stunde,
                    td [A.class "klasse expanded"] <| showVertreter hour.vertreter,
                    td [A.class "klasse expanded"] <| showFach hour.fach,
                    td [A.class "klasse expanded"] <| showRaum hour.raum,
                    td [A.class "klasse expanded"] <| showVText hour.vtext
                ]
            )
        klasse.hours

viewMOTD : Model -> UntisDay -> Html Msg
viewMOTD model day =
    ul [A.class "motd"] <|
        li [A.class "motd header"] [
            button [A.class "motd header", E.onClick (UpdateMOTDExpansion (not model.expandedMOTD))] [text <| (if model.expandedMOTD then "⯆" else "➤") ++ " Nachrichten zum Tag" ]
        ]::(if model.expandedMOTD then List.map (\m -> li [A.class "motd elem"] [text m]) day.motd else [])

viewFeedback : Model -> Html Msg
viewFeedback model = div [] [
        button [A.class "feedback back", onClick (SelectFeedback False)] [text "<— Zurück"],
        div [] [
            textarea [A.class "feedback", A.value model.feedbackText, E.onInput UpdateFeedbackText] []
        ],
        button [A.class "feedback send", onClick SendFeedback] [text "Abschicken"]

    ]
