module Getto.Url.Query.Encode exposing
  ( Value
  , string
  , int
  , bool
  , list
  , set
  , object
  , null
  , encode
  , toName
  )

{-| encode url query string

    roles = [ "admin" ] |> Set.fromList
    comments =
      [ "good"
      , "great"
      ]

    [ ( "q"
      , [ ( "name", "name" |> QueryEncode.string )
        , ( "count",     1 |> QueryEncode.int )
        , ( "active", True |> QueryEncode.bool )
        , ( "roles",   roles    |> QueryEncode.set  QueryEncode.string )
        , ( "comments" comments |> QueryEncode.list QueryEncode.string )
        ] |> QueryEncode.object
      )
    ] |> QueryEncode.object

# Definition
@docs Value

# Encoders
@docs string, int, bool, list, set, object, null

# Encode
@docs encode

# Helper
@docs toName
 -}


import Url
import Set exposing ( Set )


{-| encoded value
 -}
type Value
  = ValueEntry String
  | ValueBool Bool
  | ValueGroup (List ( String, Value ))


{-| string encoder
 -}
string : String -> Value
string = Url.percentEncode >> ValueEntry


{-| int encoder
 -}
int : Int -> Value
int = String.fromInt >> ValueEntry


{-| bool encoder
 -}
bool : Bool -> Value
bool = ValueBool


{-| list encoder
 -}
list : (a -> Value) -> List a -> Value
list encoder = List.map (\value -> ("", value |> encoder)) >> ValueGroup


{-| set encoder
 -}
set : (a -> Value) -> Set a -> Value
set encoder = Set.toList >> list encoder


{-| object encoder
 -}
object : List ( String, Value ) -> Value
object = List.map (\(name, value) -> ( name, value ) ) >> ValueGroup


{-| null encoder
 -}
null : Value
null = [] |> object


{-| convert encoded value to string
 -}
encode : Value -> String
encode value = toQuery <|
  case value of
    ValueEntry val  -> [ val ]
    ValueBool  val  -> []
    ValueGroup vals ->
      vals
      |> List.concatMap (flatten [])
      |> List.map toPair

toQuery : List String -> String
toQuery params =
  if params |> List.isEmpty
    then ""
    else "?" ++ (params |> String.join "&")

flatten : List String -> ( String, Value ) -> List ( List String, Maybe String )
flatten parents (name, value) =
  let
    current = parents ++ [ name ]
  in
    case value of
      ValueEntry val  -> [ ( current, Just val ) ]
      ValueBool  val  -> if val then [ ( current, Nothing ) ] else []
      ValueGroup vals -> vals |> List.concatMap (flatten current)

toPair : ( List String, Maybe String ) -> String
toPair (names,val) =
  (names |> toName |> Url.percentEncode) ++
  (case val of
    Just value -> "=" ++ value
    Nothing    -> ""
  )


{-| encode name

    [ "q", "name" ] |> QueryEncode.toName
    -- "q[name]"
 -}
toName : List String -> String
toName names =
  case names |> List.map Url.percentEncode of
    [] -> ""
    head :: tail ->
      head ++
      ( tail
        |> List.map (\tip -> "[" ++ tip ++ "]")
        |> String.join ""
      )
