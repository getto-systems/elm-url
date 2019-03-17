module Getto.Url.Query.SafeDecodeTest exposing (..)
import Getto.Url.Query.SafeDecode as QuerySafeDecode

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
          , ( "q[nums][]", "value" )   |> param
          , ( "sort", "name.desc" )    |> param
          ]
          |>
            (\query ->
              { query =
                { name     = query |> QuerySafeDecode.entryAt ["q","[]"]   (QuerySafeDecode.string "")
                , none     = query |> QuerySafeDecode.entryAt ["q","none"] (QuerySafeDecode.string "")
                , age      = query |> QuerySafeDecode.entryAt ["q","age"]  (QuerySafeDecode.int 0)
                , zero     = query |> QuerySafeDecode.entryAt ["q","zero"] (QuerySafeDecode.int 0)
                , enabled  = query |> QuerySafeDecode.boolAt  ["q","en"]
                , disabled = query |> QuerySafeDecode.boolAt  ["q","dis"]
                , roles    = query |> QuerySafeDecode.listAt  ["q","roles"] (QuerySafeDecode.string "")
                , numbers  = query |> QuerySafeDecode.listAt  ["q","nums"]  (QuerySafeDecode.int 0)
                , empty    = query |> QuerySafeDecode.listAt  ["q","empty"] (QuerySafeDecode.string "")
                }
              , sort = query |> QuerySafeDecode.entryAt ["sort"] (QuerySafeDecode.string "")
              }
            )
          |> Expect.equal
            { query =
              { name     = "John"
              , none     = ""
              , age      = 30
              , zero     = 0
              , enabled  = True
              , disabled = False
              , roles    = ["admin","system"]
              , numbers  = [1,0]
              , empty    = []
              }
            , sort = "name.desc"
            }

      , test "should decode simple string" <|
        \_ ->
          Just "value"
          |> QuerySafeDecode.string ""
          |> Expect.equal "value"

      , test "should return default string with nothing" <|
        \_ ->
          Nothing
          |> QuerySafeDecode.string ""
          |> Expect.equal ""

      , test "should decode simple int" <|
        \_ ->
          Just "12"
          |> QuerySafeDecode.int 0
          |> Expect.equal 12

      , test "should return default int with nothing" <|
        \_ ->
          Nothing
          |> QuerySafeDecode.int 0
          |> Expect.equal 0

      , test "should return empty dict if decode failed" <|
        \_ ->
          Just ""
          |> QuerySafeDecode.int 0
          |> Expect.equal 0

      , test "should decode boolean true" <|
        \_ ->
          ["value"]
          |> (QuerySafeDecode.boolAt ["value"])
          |> Expect.equal True

      , test "should decode boolean false" <|
        \_ ->
          [""]
          |> (QuerySafeDecode.boolAt ["value"])
          |> Expect.equal False

      , test "should decode special chars" <|
        \_ ->
          [ ( "?[ ]=&" |> Url.percentEncode, "[ ]=&?" ) |> param ]
          |> (QuerySafeDecode.entryAt ["?[ ]=&"] (QuerySafeDecode.string ""))
          |> Expect.equal "[ ]=&?"

      , test "should decode first entry with several entries" <|
        \_ ->
          [ "entry=value"
          , "entry=value2"
          , "entry=value3"
          ]
          |> (QuerySafeDecode.entryAt ["entry"] (QuerySafeDecode.string ""))
          |> Expect.equal "value"

      , test "should decode first entry with several entries that decode successful or failed" <|
        \_ ->
          [ "number=value"
          , "number=2"
          , "number=3"
          ]
          |> (QuerySafeDecode.entryAt ["number"] (QuerySafeDecode.int 0))
          |> Expect.equal 0
      ]
    ]
