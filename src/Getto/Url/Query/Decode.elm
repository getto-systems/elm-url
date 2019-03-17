module Getto.Url.Query.Decode exposing
  ( Value
  , Decoder
  , string
  , int
  , entryAt
  , listAt
  , boolAt
  , split
  )

{-| decode url query string

    value = "count=4&position[]=captain&admin" |> QueryDecode.split

    value |> QueryDecode.entryAt ["count"]    QueryDecode.int
    value |> QueryDecode.listAt  ["position"] QueryDecode.string
    value |> QueryDecode.boolAt  ["admin"]

# Definition
@docs Value, Decoder

# Decoders
@docs string, int

# Decode
@docs entryAt, listAt, boolAt

# Helper
@docs split
 -}


import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.DecodeHelper as QueryDecodeHelper

import Url


{-| decode target : actually this is List String
 -}
type alias Value = List String


{-| decoder : Value -> Maybe a
 -}
type alias Decoder a = Value -> Maybe a
type alias ValueDecoder a = String -> Maybe a


{-| string decoder
 -}
string : ValueDecoder String
string = Url.percentDecode


{-| int decoder
 -}
int : ValueDecoder Int
int = String.toInt


{-| decode entry
 -}
entryAt : List String -> ValueDecoder a -> Decoder a
entryAt names decoder =
  QueryDecodeHelper.filter names ""
  >> List.head
  >> Maybe.andThen decoder


{-| decode list
 -}
listAt : List String -> ValueDecoder a -> Decoder (List a)
listAt names decoder list =
  case list |> QueryDecodeHelper.filter names "[]" of
    [] -> Nothing
    values ->
      let
        decoded = values |> List.map decoder
      in
        if decoded |> List.any ((==) Nothing)
          then Nothing
          else decoded |> List.filterMap identity |> Just


{-| decode bool
 -}
boolAt : Value -> Decoder Bool
boolAt names = QueryDecodeHelper.find names >> Just


{-| query string to Value

    "name=John&position=captain" |> QueryDecode.split
    -- [ "name=John", "position=captain" ]
 -}
split : String -> Value
split = String.split "&"
