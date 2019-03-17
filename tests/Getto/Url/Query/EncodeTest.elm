module Getto.Url.Query.EncodeTest exposing (..)
import Getto.Url.Query.Encode as Encode

import Set
import Url

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

param ( name, value ) = (name |> Url.percentEncode) ++ "=" ++ (value |> Url.percentEncode)
bool = Url.percentEncode

suite : Test
suite =
  describe "Encode"
    [ describe "encode"
      [ test "should encode params" <|
        \_ ->
          let
            value =
              [ ( "q"
                , [ ( "name",    "John" |> Encode.string )
                  , ( "age",     30     |> Encode.int )
                  , ( "enabled", True   |> Encode.bool )
                  , ( "roles"
                    , [ "admin"
                      , "system"
                      ] |> Set.fromList |> Encode.set Encode.string
                    )
                  ] |> Encode.object
                )
              , ( "sort", "name.desc" |> Encode.string )
              ] |> Encode.object
          in
            value |> Encode.encode
            |> Expect.equal
              ( "?" ++
                ( [ ( "q[name]", "John" ) |> param
                  , ( "q[age]",  "30" )   |> param
                  , ( "q[enabled]" )      |> bool
                  , ( "q[roles][]", "admin" )  |> param
                  , ( "q[roles][]", "system" ) |> param
                  , ( "sort", "name.desc" ) |> param
                  ] |> String.join "&"
                )
              )

      , test "should encode simple string" <|
        \_ ->
          let
            value = "value" |> Encode.string
          in
            value |> Encode.encode
            |> Expect.equal "?value"

      , test "should encode simple int" <|
        \_ ->
          let
            value = 12 |> Encode.int
          in
            value |> Encode.encode
            |> Expect.equal "?12"

      , test "should nothing with simple boolean true" <|
        \_ ->
          let
            value = True |> Encode.bool
          in
            value |> Encode.encode
            |> Expect.equal ""

      , test "should nothing with simple boolean false" <|
        \_ ->
          let
            value = False |> Encode.bool
          in
            value |> Encode.encode
            |> Expect.equal ""

      , test "should encode boolean true" <|
        \_ ->
          let
            value = [ ( "value", True |> Encode.bool ) ] |> Encode.object
          in
            value |> Encode.encode
            |> Expect.equal "?value"

      , test "should encode boolean false" <|
        \_ ->
          let
            value = [ ( "value", False |> Encode.bool ) ] |> Encode.object
          in
            value |> Encode.encode
            |> Expect.equal ""

      , test "should return empty string if null" <|
        \_ ->
          let
            value = Encode.null
          in
            value |> Encode.encode
            |> Expect.equal ""

      , test "should escape special chars" <|
        \_ ->
          let
            value = [ ( "?[ ]=&", "[ ]=&?" |> Encode.string ) ] |> Encode.object
          in
            value |> Encode.encode
            |> Expect.equal
              ( "?" ++
                ( [ ( "?[ ]=&" |> Url.percentEncode, "[ ]=&?" ) |> param
                  ] |> String.join "&"
                )
              )
      ]
    ]
