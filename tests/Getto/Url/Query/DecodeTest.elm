module Getto.Url.Query.DecodeTest exposing (..)
import Getto.Url.Query.Decode as QueryDecode

import Url

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

param ( name, value ) = (name |> Url.percentEncode) ++ "=" ++ (value |> Url.percentEncode)
bool = Url.percentEncode

suite : Test
suite =
  describe "Decode"
    [ describe "decode"
      [ test "should decode query" <|
        \_ ->
          [ ( "q[%5B%5D]", "John" )    |> param
          , ( "q[age]", "30" )         |> param
          , ( "q[en]" )                |> bool
          , ( "q[roles][]", "admin" )  |> param
          , ( "q[roles][]", "system" ) |> param
          , ( "q[nums][]", "1" )       |> param
          , ( "q[nums][]", "2" )       |> param
          , ( "q[fails][]", "1" )      |> param
          , ( "q[fails][]", "value" )  |> param
          , ( "sort", "name.desc" )    |> param
          ]
          |>
            (\query ->
              { query =
                { name     = query |> QueryDecode.entryAt ["q","[]"]   QueryDecode.string
                , none     = query |> QueryDecode.entryAt ["q","none"] QueryDecode.string
                , age      = query |> QueryDecode.entryAt ["q","age"]  QueryDecode.int
                , zero     = query |> QueryDecode.entryAt ["q","zero"] QueryDecode.int
                , enabled  = query |> QueryDecode.boolAt  ["q","en"]
                , disabled = query |> QueryDecode.boolAt  ["q","dis"]
                , roles    = query |> QueryDecode.listAt  ["q","roles"] QueryDecode.string
                , numbers  = query |> QueryDecode.listAt  ["q","nums"]  QueryDecode.int
                , fails    = query |> QueryDecode.listAt  ["q","fails"] QueryDecode.int
                , empty    = query |> QueryDecode.listAt  ["q","empty"] QueryDecode.string
                }
              , sort = query |> QueryDecode.entryAt ["sort"] QueryDecode.string
              }
            )
          |> Expect.equal
            { query =
              { name     = Just "John"
              , none     = Nothing
              , age      = Just 30
              , zero     = Nothing
              , enabled  = Just True
              , disabled = Just False
              , roles    = Just ["admin","system"]
              , numbers  = Just [1,2]
              , fails    = Nothing
              , empty    = Nothing
              }
            , sort = Just "name.desc"
            }

      , test "should decode simple string" <|
        \_ ->
          "value"
          |> QueryDecode.string
          |> Expect.equal (Just "value")

      , test "should decode simple int" <|
        \_ ->
          "12"
          |> QueryDecode.int
          |> Expect.equal (Just 12)

      , test "should return Nothing if decode failed" <|
        \_ ->
          ""
          |> QueryDecode.int
          |> Expect.equal Nothing

      , test "should decode boolean true" <|
        \_ ->
          ["value"]
          |> (QueryDecode.boolAt ["value"])
          |> Expect.equal (Just True)

      , test "should decode boolean false" <|
        \_ ->
          [""]
          |> (QueryDecode.boolAt ["value"])
          |> Expect.equal (Just False)

      , test "should decode special chars" <|
        \_ ->
          [ ( "?[ ]=&" |> Url.percentEncode, "[ ]=&?" ) |> param ]
          |> (QueryDecode.entryAt ["?[ ]=&"] QueryDecode.string)
          |> Expect.equal (Just "[ ]=&?")

      , test "should decode first entry with several entries" <|
        \_ ->
          [ "entry=value"
          , "entry=value2"
          , "entry=value3"
          ]
          |> (QueryDecode.entryAt ["entry"] QueryDecode.string)
          |> Expect.equal (Just "value")

      , test "should decode first entry with several entries that decode successful or failed" <|
        \_ ->
          [ "number=value"
          , "number=2"
          , "number=3"
          ]
          |> (QueryDecode.entryAt ["number"] QueryDecode.int)
          |> Expect.equal Nothing
      ]
    ]
