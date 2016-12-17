module Example exposing (..)

import Html.App as Html
import Html exposing (..)
import String
import Task exposing (fail, succeed, Task)
import Task.Path exposing (..)
import Html.Events exposing (onFocus, onInput, onBlur, onClick)


type Msg
    = TrySomeTask
    | Cakes (List String)
    | Error String


type alias Model =
    { cakes : List String
    , error : String
    }



--


appender : String -> String -> Task x String
appender s1 s2 =
    succeed <| s1 ++ s2 ++ s1


sideEffect : String -> Task x String
sideEffect s =
    succeed (Debug.log "sideEffect" s)


splitter : String -> String -> Task x (List String)
splitter sp s =
    succeed <| String.split sp s


failer : String -> Task String x
failer s =
    fail "Oh well"


someTask : Task String (List String)
someTask =
    succeed "Hollohhh"
        >>=> appender "====="
        >>=> appender "++"
        >>=> (failer <|>> splitter "o")


otherTask : Task String (List String)
otherTask =
    succeed "Hollohhh"
        >>=> appender "====="
        >>=> appender "++"
        >>=> (failer <|>> splitter "o")



--
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrySomeTask ->
            model ! [ Task.perform Error Cakes someTask ]

        Cakes ss ->
            { model | cakes = ss } ! []

        Error s ->
            { model | error = s } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "cakes:" ]
        , div [] [ text <| toString model.cakes ]
        , h3 [] [ text "error:" ]
        , div [] [ text <| toString model.error ]
        , button [ onClick TrySomeTask ] [ text "Click me, bad boy" ]
        ]



-- APP


main : Program Never
main =
    Html.program
        { init = { cakes = [], error = "" } ! []
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
