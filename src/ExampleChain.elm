module ExampleChain exposing (..)

import Chain exposing (Chain(Success, Failure), (>>=))
import Html.App as Html
import Html exposing (..)
import String
import Task exposing (fail, succeed, Task)
import Task.Path exposing (..)
import Html.Events exposing (onFocus, onInput, onBlur, onClick)
import Random
import Updater exposing (toCmd, converter, Updater, Converter)


type Msg
    = TrySomeTask
    | TrySomeCmd
    | Cakes (List String)
    | Error String
    | Updater (Updater Model Msg)


type alias Model =
    { cakes : List String
    , error : String
    }



--


appender : String -> String -> Task x String
appender s1 s2 =
    succeed <| s1 ++ s2 ++ s1


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


appenderCmd : String -> String -> Cmd (Chain x String)
appenderCmd s1 s2 =
    Chain.fromTask <| appender s1 s2


addRandomInt : String -> Cmd (Chain x String)
addRandomInt s =
    Random.generate Success (Random.int 0 999)
        >>= (\n -> appenderCmd (toString n) s)


splitterCmd : String -> String -> Cmd (Chain x (List String))
splitterCmd s1 s2 =
    Chain.fromTask <| splitter s1 s2


failerCmd : String -> Cmd (Chain String x)
failerCmd s =
    Chain.fromTask <| failer s


someTaskCmd : Cmd (Chain String (List String))
someTaskCmd =
    Chain.fromTask someTask


otherTaskCmd : Cmd (Chain String (List String))
otherTaskCmd =
    Chain.pure "Hollohhh"
        >>= appenderCmd "==="
        >>= addRandomInt
        >>= appenderCmd "++"
        >>= splitterCmd "o"



--


repeatStringCommand : Int -> String -> Cmd (Chain x String)
repeatStringCommand n s =
    Task.perform Failure Success (succeed <| String.repeat n s)



--
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrySomeTask ->
            model ! [ Task.perform Error Cakes someTask ]

        TrySomeCmd ->
            model ! [ Chain.perform Updater Error Cakes otherTaskCmd ]

        Cakes ss ->
            { model | cakes = ss } ! []

        Error s ->
            { model | error = s } ! []

        Updater u ->
            u model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "cakes:" ]
        , div [] [ text <| toString model.cakes ]
        , h3 [] [ text "error:" ]
        , div [] [ text <| toString model.error ]
        , button [ onClick TrySomeTask ] [ text "Take me to task, bad boy" ]
        , button [ onClick TrySomeCmd ] [ text "COMMAND me, bad boy" ]
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
