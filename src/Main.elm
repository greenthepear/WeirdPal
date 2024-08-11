module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex



-- MAIN


main : Program () Model Msg
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
    , fullParody : String
    }


init : Model
init =
    { content =
        Dict.fromList
            [ ( 0, Line 0 "This is a placeholder!" "" )
            , ( 1, Line 1 "Add the original lyrics on the right" "" )
            ]
    , fullParody = ""
    }



-- UPDATE


type Msg
    = Change Int String
    | ChangeOriginal Int String
    | ChangeOriginalFromMultiline String
    | ChangeParodyFromMultiline String


syllable : Regex.Regex
syllable =
    Maybe.withDefault Regex.never <|
        Regex.fromString "([aeiouyAEIOUY]+[^e.\\s])|([aiouyAEIOUY]+\\b)|(\\b[^aeiouy0-9.']+e\\b)"


syllableCount : String -> Int
syllableCount str =
    Regex.find syllable str |> List.length


changeOriginal : Dict Int Line -> Int -> String -> Dict Int Line
changeOriginal dict lineNum newString =
    Dict.update lineNum
        (\maybeLine ->
            case maybeLine of
                Just line ->
                    Just { line | original = newString }

                Nothing ->
                    Just { number = lineNum, original = newString, parody = "" }
        )
        dict


changeParody : Dict Int Line -> Int -> String -> Dict Int Line
changeParody dict lineNum newString =
    Dict.update lineNum
        (\maybeLine ->
            case maybeLine of
                Just line ->
                    Just { line | parody = newString }

                Nothing ->
                    Just { number = lineNum, original = "", parody = newString }
        )
        dict


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change lineNum newString ->
            let
                newDict =
                    Dict.update lineNum
                        (\maybeLine ->
                            case maybeLine of
                                Just line ->
                                    Just { line | parody = newString }

                                Nothing ->
                                    Just { number = lineNum, parody = newString, original = "" }
                        )
                        model.content
            in
            { model
                | content = newDict
                , fullParody =
                    Dict.foldl
                        (\_ line full -> full ++ line.parody ++ "\n")
                        ""
                        newDict
            }

        ChangeOriginal lineNum newString ->
            { model
                | content = changeOriginal model.content lineNum newString
            }

        ChangeOriginalFromMultiline str ->
            { model
                | content =
                    String.lines str
                        |> List.indexedMap Tuple.pair
                        |> List.foldl (\( i, line ) d -> changeOriginal d i line) model.content
            }

        ChangeParodyFromMultiline str ->
            { model
                | content =
                    String.lines str
                        |> List.indexedMap Tuple.pair
                        |> List.foldl (\( i, line ) d -> changeParody d i line) model.content
                , fullParody = str
            }



-- VIEW


lineCountStyle : List (Attribute msg)
lineCountStyle =
    [ style "justify-self" "center"
    , style "text-align" "center"
    ]


containerStyle : List (Attribute msg)
containerStyle =
    [ style "display" "flex"
    , style "justify-content" "space-between"
    , style "position" "absolute"
    , style "width" "100%"
    , style "height" "100%"
    ]


rightContainerStyle : List (Attribute msg)
rightContainerStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "space-between"
    , style "width" "50%"
    , style "height" "100%"
    , style "border" "2px solid black"
    ]


makeSongLine : Line -> List (Html Msg)
makeSongLine ln =
    [ div lineCountStyle [ text (syllableCount ln.original |> String.fromInt) ]
    , div [] [ input [ value ln.original, disabled True, style "width" "98%" ] [] ]
    , div lineCountStyle [ text <| String.fromInt <| syllableCount <| ln.parody ]
    , div [] [ input [ value ln.parody, placeholder "...", onInput (Change ln.number), style "width" "98%" ] [] ]
    ]


makeSongLineDiv : Line -> Html Msg
makeSongLineDiv ln =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr 20fr"
        , style "padding-top" "1%"
        , style "width" "100%"
        ]
        (makeSongLine ln)


view : Model -> Html Msg
view model =
    div containerStyle
        [ div [ style "width" "60%", style "overflow-y" "scroll" ]
            (Dict.values
                model.content
                |> List.map makeSongLineDiv
            )
        , div rightContainerStyle
            [ div [ style "height" "95%", style "padding" "0% 2% 2% 2%" ]
                [ p [] [ text "Original:" ]
                , textarea
                    [ onInput ChangeOriginalFromMultiline
                    , style "width" "100%"
                    , style "height" "50%"
                    ]
                    []
                ]
            , div [ style "height" "95%", style "padding" "2% 2% 2% 2%" ]
                [ p [] [ text "Changed:" ]
                , textarea
                    [ onInput ChangeParodyFromMultiline
                    , value model.fullParody
                    , style "width" "100%"
                    , style "height" "50%"
                    ]
                    []
                ]
            ]
        ]
