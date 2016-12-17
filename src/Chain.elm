module Chain exposing (Chain(Success, Failure), fromTask, (>>=), perform, pure, fail)

import Updater exposing (toCmd, converter, Updater, Converter)
import Task exposing (Task)


type Chain x a
    = Success a
    | Failure x
    | Updater (Updater Model (Chain x a))


fromTask : Task x a -> Cmd (Chain x a)
fromTask =
    Task.perform Failure Success


type Model
    = Unused


model : Model
model =
    Unused



--


pure : a -> Cmd (Chain x a)
pure a =
    toCmd <| Success a



--


fail : x -> Cmd (Chain x a)
fail x =
    toCmd <| Failure x



--


update : Chain x a -> Model -> ( Model, Cmd (Chain x a) )
update msg model =
    case msg of
        Updater u ->
            u model

        _ ->
            model ! []



--


chainConverter : (b -> Cmd (Chain x c)) -> Converter (Chain x c) (Chain x b)
chainConverter fbc =
    converter
        Updater
        { get = \m -> Just m
        , set = \cm m -> m
        , update = update
        , react =
            \cMsg _ model ->
                case cMsg of
                    Success a ->
                        ( model, fbc a )

                    Failure x ->
                        ( model, toCmd <| Failure x )

                    _ ->
                        model ! []
        }


(==>) : (a -> Cmd (Chain x b)) -> (b -> Cmd (Chain x c)) -> (a -> Cmd (Chain x c))
(==>) fab fbc =
    \a -> Cmd.map (chainConverter fbc) <| fab a


(>>=) : Cmd (Chain x a) -> (a -> Cmd (Chain x b)) -> Cmd (Chain x b)
(>>=) aChain fab =
    Cmd.map (chainConverter fab) <| aChain


type alias UpdaterMsg pModel pMsg =
    Updater pModel pMsg -> pMsg


perform :
    UpdaterMsg pModel msg
    -> (x -> msg)
    -> (a -> msg)
    -> Cmd (Chain x a)
    -> Cmd msg
perform updaterMsg failMsg successMsg =
    let
        c =
            converter
                updaterMsg
                { get = \m -> Just Unused
                , set = \cm m -> m
                , update = update
                , react =
                    \cMsg _ model ->
                        case cMsg of
                            Success a ->
                                model ! [ toCmd <| successMsg a ]

                            Failure s ->
                                model ! [ toCmd <| failMsg s ]

                            _ ->
                                model ! []
                }
    in
        Cmd.map c
