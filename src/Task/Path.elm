module Task.Path exposing (..)

import Task exposing (fail, succeed, andThen, Task, perform, onError, map)


(>>=>) : Task x a -> (a -> Task x b) -> Task x b
(>>=>) =
    Task.andThen


(<|>>) : (a -> Task x b) -> (a -> Task x b) -> (a -> Task x b)
(<|>>) tf1 tf2 =
    \a ->
        onError (tf1 a) (always (tf2 a))


(<*>>) : Task x (a -> b) -> Task x a -> Task x b
(<*>>) tab ta =
    tab >>=> (\ab -> map ab ta)



--(*>>) : (a -> Task x b -> Task x b -> Task x b
{- infixl 3 >>= -}
