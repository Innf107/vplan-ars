module VPlanTypes exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Encode as JE
import Http
import Html as H
import Html.Events as E
import Html.Attributes as A
import Parser exposing (..)
import Set

type alias UntisData = {
        vplan: VPlan
    }

type alias VPlan = List UntisDay

type alias UntisDay = {
        day: String,
        klassen: List UntisKlasse,
        motd: List String
    }

type alias UntisKlasse = {
        name: String,
        hours: List UntisHour
    }

type alias UntisHour = {
        stunde: String,
        vertreter: String,
        fach: String,
        raum: String,
        vtext: String
    }

uHourDecoder : Decoder UntisHour
uHourDecoder = D.map5 UntisHour
                (D.field "stunde" D.string)
                (D.field "vertreter" D.string)
                (D.field "fach" D.string)
                (D.field "raum" D.string)
                (D.field "vtext" D.string)

uKlasseDecoder : Decoder UntisKlasse
uKlasseDecoder = D.map2 UntisKlasse
                (D.field "name" D.string)
                (D.field "hours" (D.list uHourDecoder))

uDayDecoder : Decoder UntisDay
uDayDecoder = D.map3 UntisDay
                (D.field "day" D.string)
                (D.field "klassen" (D.list uKlasseDecoder))
                (D.at ["motd", "content"] (D.list D.string))

uDataDecoder : Decoder UntisData
uDataDecoder = D.map UntisData
                    (D.field "vplan" (D.list uDayDecoder))

errToStr : Http.Error -> String
errToStr e = case e of
    Http.BadUrl str -> "BadUrl (" ++ str ++ ")"
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "NetworkError"
    Http.BadStatus int -> "BadStatus (" ++ String.fromInt int ++ ")"
    Http.BadBody str -> "BadBody (" ++ str ++ ")"

getAt : Int -> List a -> Maybe a
getAt a b = case (a,b) of
    (_, [])    -> Nothing
    (0, x::_) -> Just x
    (n, _::xs) -> getAt (n - 1) xs

link : List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg
link attributes children = H.node "link" attributes children

delete : a -> List a -> List a
delete a b = case (a, b) of
    (_, []) -> []
    (x, y::ys) -> if x == y
        then ys
        else y::(delete x ys)

onClick : msg -> H.Attribute msg
onClick msg = E.on "click" (D.succeed msg)

showStunde : String -> List (H.Html msg)
showStunde txt = [H.text txt]

showVertreter : String -> (List (H.Html msg))
showVertreter txt = case (parseVertreter txt) of
    Err _ -> [H.pre [] [H.text "An error occured!"]]
    Ok  v -> case v of
        VertreterPlain x     -> [H.p [A.class "vertreter plain"] [H.text x]]
        VertreterStrike s    -> [H.s [A.class "vertreter strike"] [H.text s]]
        VertreterReplace x y -> [H.s [A.class "vertreter strike replace"] [H.text x],
                        H.strong [A.class "vertreter arrow replace"] [H.text " —> "],
                        H.strong [A.class "vertreter second replace"] [H.text y]]

showFach : String -> List (H.Html msg)
showFach txt = case (parseFach txt) of
    Err _ -> [H.pre [] [H.text "An error occured!"]]
    Ok f -> case f of
        FachPlain x  -> [H.p [A.class "fach plain"]  [H.text x]]
        FachStrike s -> [H.s [A.class "fach strike"] [H.text s]]

showRaum : String -> List (H.Html msg)
showRaum txt = case parseRaum txt of
    Err _ -> [H.pre [] [H.text "An error occured!"]]
    Ok r -> case r of
        RaumEmpty       -> [H.p [A.class "raum empty"]  [H.text " — "]]
        RaumPlain x     -> [H.p [A.class "raum plain"]  [H.text x]]
        RaumStrike x    -> [H.s [A.class "raum strike"] [H.text x]]
        RaumReplace x y -> [H.s [A.class "raum strike replace"] [H.text x],
                            H.strong [A.class "raum arrow replace"] [H.text " —> "],
                            H.strong [A.class "raum second replace"] [H.text y]]

showVText : String -> List (H.Html msg)
showVText txt = case parseVText txt of
    Err _ -> [H.pre [] [H.text "An error occured!"]]
    Ok t -> case t of
        VTEmpty   -> [H.p [A.class "vtext empty"]  [H.text " — "]]
        VTPlain x -> [H.p [A.class "vtext plain"]  [H.text x]]

type VertreterVariant = VertreterPlain String
                      | VertreterStrike String
                      | VertreterReplace String String

parseVertreter : String -> Result (List DeadEnd) VertreterVariant
parseVertreter = run <| oneOf
                     [
                        backtrackable <| succeed VertreterReplace
                        |. spaces
                        |. symbol "<s>"
                        |. spaces
                        |= parseStr
                        |. symbol "</s>"
                        |. symbol "?"
                        |= parseStr,

                        succeed VertreterStrike
                        |. spaces
                        |. symbol "<s>"
                        |. spaces
                        |= parseStr
                        |. spaces
                        |. symbol "</s>",

                        succeed  VertreterPlain
                        |= parseStr
                     ]

type FachVariant = FachPlain String
                 | FachStrike String

parseFach : String -> Result (List DeadEnd) FachVariant
parseFach = run <| oneOf
                [
                    backtrackable <| succeed FachStrike
                    |. spaces
                    |. symbol "<s>"
                    |. spaces
                    |= parseStr
                    |. spaces
                    |. symbol "</s>",

                    backtrackable <| succeed FachPlain
                    |= parseStr
                ]

type RaumVariant = RaumPlain String
                 | RaumReplace String String
                 | RaumStrike String
                 | RaumEmpty

parseRaum = run <| oneOf
                 [
                    backtrackable <| succeed RaumEmpty
                    |. spaces
                    |. symbol "---"
                    |. spaces,

                    backtrackable <| succeed RaumReplace
                    |. spaces
                    |. symbol "<s>"
                    |. spaces
                    |= parseStr
                    |. spaces
                    |. symbol "</s>"
                    |. symbol "?"
                    |= parseStr,

                    backtrackable <| succeed RaumStrike
                    |. spaces
                    |. symbol "<s>"
                    |. spaces
                    |= parseStr
                    |. spaces
                    |. symbol "</s>",

                    backtrackable <| succeed RaumPlain
                    |= parseStr
                 ]

type VTextVariant = VTPlain String
                  | VTEmpty

parseVText : String -> Result (List DeadEnd) VTextVariant
parseVText = run <| oneOf
                [
                    succeed VTEmpty
                    |. symbol "&nbsp;",

                    succeed VTEmpty
                    |. spaces
                    |. end,

                    succeed VTPlain
                    |. spaces
                    |= parseID
                    |.spaces
                ]

parseID = getChompedString <| chompWhile (\_ -> True)

parseStr = getChompedString <| chompWhile (\x -> Char.isAlphaNum x || String.contains (String.fromChar x) "ÄÖÜäöüß.")


listToStr : (a -> String) -> List a -> String
listToStr f xs = "[" ++ String.join "," (List.map f xs) ++ "]"


isWhiteSpace : String -> Bool
isWhiteSpace = String.isEmpty << String.trim

find : (a -> Bool) -> List a -> Maybe a
find f l = case l of
    [] -> Nothing
    (x::xs) -> if f x
               then Just x
               else find f xs

getHoursWithTeacher : UntisDay -> String -> List UntisKlasse
getHoursWithTeacher day str = day.klassen                                                          -- TODO
                                |> List.filter  (\k -> List.any
                                    (\x -> vertreterIsEqual (parseVertreter x.vertreter) str
                                    ) k.hours)
                                |> List.map
                                    (\k -> {k|hours=List.filter
                                        (\h ->vertreterIsEqual (parseVertreter h.vertreter) str) k.hours})

vertreterIsEqual : Result (List DeadEnd) VertreterVariant -> String -> Bool
vertreterIsEqual y str = case y of
     Err _ -> False
     Ok z -> case z of
         VertreterPlain v -> v == str
         VertreterStrike s -> s == str
         VertreterReplace f t -> f == str || t == str