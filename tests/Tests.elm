module Tests exposing (suite)

import Bytes.Encode exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Proto
import Test exposing (..)


suite : Test
suite =
    describe "The Proto module"
        [ describe "parseVarint"
            [ test "parses 1" <|
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0x01
                                ]
                                |> encode
                    in
                    Expect.equal 1 (Proto.parseVarint bytes)
            , test "parses 300" <|
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0xAC
                                , unsignedInt8 0x02
                                ]
                                |> encode
                    in
                    Expect.equal 300 (Proto.parseVarint bytes)
            ]
        ]