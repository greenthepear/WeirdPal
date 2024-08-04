module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Line =
    { number : Int
    , original : String
    , parody : String
    }


type alias Model =
    { content : Dict Int Line
    }


init : Model
init =
    { content =
        Dict.fromList [ ( 0, Line 0 "I am a sinner, who's probably gonna sin again" "" ) ]
    }



-- UPDATE


type Msg
    = Change Int String


syllable : Regex.Regex
syllable =
    Maybe.withDefault Regex.never <|
        Regex.fromString "([aeiouyAEIOUY]+[^e.\\s])|([aiouyAEIOUY]+\\b)|(\\b[^aeiouy0-9.']+e\\b)"


syllableCount : String -> Int
syllableCount str =
    Regex.find syllable str |> List.length


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change lineNum newString ->
            { model | content = Dict.update lineNum (Maybe.map (\line -> { line | parody = newString })) model.content }



-- VIEW


lineCountStyle : List (Attribute msg)
lineCountStyle =
    [ style "justify-self" "center", style "text-align" "center" ]


makeSongLine : Line -> List (Html Msg)
makeSongLine ln =
    [ div lineCountStyle [ text (syllableCount ln.original |> String.fromInt) ]
    , div [] [ input [ value ln.original, disabled True, style "width" "100%" ] [] ]
    , div lineCountStyle [ text <| String.fromInt <| syllableCount <| ln.parody ]
    , div [] [ input [ placeholder "...", onInput (Change ln.number), style "width" "100%" ] [] ]
    ]


view : Model -> Html Msg
view model =
    div [ style "display" "grid", style "grid-template-columns" "1fr 60fr" ]
        (makeSongLine (Maybe.withDefault (Line 0 "" "") (Dict.get 0 model.content)))
