module Getto.Url.Query.EncodeTest exposing (..)
import Getto.Url.Query.Encode as Encode

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

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
              , ( "s", "name.desc" |> Encode.string )
              ] |> Encode.object
          in
            value |> Encode.encode
            |> Expect.equal "?q%5Bname%5D=John&q%5Bage%5D=30&q%5Benabled%5D&q%5Broles%5D%5B%5D=admin&q%5Broles%5D%5B%5D=system&s=name.desc"

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
            |> Expect.equal "?%253F%255B%2520%255D%253D%2526=%5B%20%5D%3D%26%3F"
      ]
    ]
