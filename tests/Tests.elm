module Tests exposing (suite)

import Bytes.Decode as Decode
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
                -- example from https://developers.google.com/protocol-buffers/docs/encoding
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0x01
                                ]
                                |> encode
                    in
                    Expect.equal (Just 1) (Decode.decode Proto.varint bytes)
            , test "parses 300" <|
                -- example from https://developers.google.com/protocol-buffers/docs/encoding
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0xAC
                                , unsignedInt8 0x02
                                ]
                                |> encode
                    in
                    Expect.equal (Just 300) (Decode.decode Proto.varint bytes)
            , test "parses 4735388" <|
                -- created with https://www.wilgysef.com/blog/varint-converter/
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0x9C
                                , unsignedInt8 0x83
                                , unsignedInt8 0xA1
                                , unsignedInt8 0x02
                                ]
                                |> encode
                    in
                    Expect.equal (Just 4735388) (Decode.decode Proto.varint bytes)
            , test "parses 86" <|
                -- created with https://www.wilgysef.com/blog/varint-converter/
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0x56
                                ]
                                |> encode
                    in
                    Expect.equal (Just 86) (Decode.decode Proto.varint bytes)
            , test "parses 3414" <|
                -- created with https://www.wilgysef.com/blog/varint-converter/
                \_ ->
                    let
                        bytes =
                            sequence
                                [ unsignedInt8 0xD6
                                , unsignedInt8 0x1A
                                , unsignedInt8 0x04
                                ]
                                |> encode
                    in
                    Expect.equal (Just 3414) (Decode.decode Proto.varint bytes)
            ]
        ]
