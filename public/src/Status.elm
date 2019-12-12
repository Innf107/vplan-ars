module Status exposing (..)

type Status a = Loading
              | Error String
              | Loaded a


map : (a -> b) -> Status a -> Status b
map f s = case s of
    Loading -> Loading
    Error e -> Error e
    Loaded x-> Loaded (f x)

andThen : (a -> Status b) -> Status a -> Status b
andThen f s = case s of
    Loading -> Loading
    Error e -> Error e
    Loaded x-> f x

apply : Status (a -> b) -> Status a -> Status b
apply sf s = sf |> andThen (\f -> s |> map f)

pure : a -> Status a
pure x = Loaded x

return : a -> Status a
return = pure

mempty : Status a
mempty = Loading

fromMaybe : Maybe a -> Status a
fromMaybe m = case m of
    Nothing -> Loading
    Just x  -> Loaded x

toMaybe : Status a -> Maybe a
toMaybe s = case s of
    Loading -> Nothing
    Error _ -> Nothing
    Loaded x-> Just x

fromResult : (a -> String) -> Result a b -> Status b
fromResult f r = case r of
    Err e -> Error <| f e
    Ok x  -> Loaded x