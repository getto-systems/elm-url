module Getto.Url.Query.DecodeHelper exposing
  ( filter
  , find
  )

import Getto.Url.Query.Encode as QueryEncode

import Url

filter : List String -> String -> List String -> List String
filter names suffix =
  let
    name = ((names |> QueryEncode.toName) ++ suffix |> Url.percentEncode) ++ "="
    length = name |> String.length
  in
    List.filterMap
      (\query ->
        if query |> String.startsWith name
          then query |> String.dropLeft length |> Just
          else Nothing
      )

find : List String -> List String -> Bool
find names = List.any ((==) (names |> QueryEncode.toName |> Url.percentEncode))
