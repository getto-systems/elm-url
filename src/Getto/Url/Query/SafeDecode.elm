module Getto.Url.Query.SafeDecode exposing
  ( Value
  , Decoder
  , string
  , int
  , entryAt
  , listAt
  , boolAt
  , split
  )

{-| decode url query string safety

    value = "count=4&position[]=captain&admin" |> QuerySafeDecode.split

    value |> QueryDecode.entryAt ["count"]    (QuerySafeDecode.int 0)
    value |> QueryDecode.listAt  ["position"] (QuerySafeDecode.string "")
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


import Getto.Url.Query.Decode as QueryDecode
import Getto.Url.Query.DecodeHelper as QueryDecodeHelper

import Url


{-| decode target : actually this is List String
 -}
type alias Value = QueryDecode.Value


{-| decoder : Value -> Maybe a
 -}
type alias Decoder a = Value -> a
type alias ValueDecoder a = Maybe String -> a


{-| string decoder
 -}
string : String -> ValueDecoder String
string default = Maybe.andThen QueryDecode.string >> Maybe.withDefault default


{-| int decoder
 -}
int : Int -> ValueDecoder Int
int default = Maybe.andThen QueryDecode.int >> Maybe.withDefault default


{-| decode entry
 -}
entryAt : List String -> ValueDecoder a -> Decoder a
entryAt names decoder =
  QueryDecodeHelper.filter names ""
  >> List.head
  >> decoder


{-| decode list
 -}
listAt : List String -> ValueDecoder a -> Decoder (List a)
listAt names decoder =
  QueryDecodeHelper.filter names "[]"
  >> List.map (Just >> decoder)


{-| decode bool
 -}
boolAt : List String -> Decoder Bool
boolAt = QueryDecodeHelper.find


{-| query string to Value

    "name=John&position=captain" |> QuerySafeDecode.split
    -- [ "name=John", "position=captain" ]
 -}
split : String -> Value
split = QueryDecode.split
