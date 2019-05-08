module Tile exposing (Tile, decode)

import Bitwise
import Browser
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Dict
import Html exposing (Html, text)
import Http
import Proto
import Task


decode : Int -> Bytes.Bytes -> Maybe Tile
decode len bs =
    Decode.decode (decoder len) bs


decoder : Int -> Decoder Tile
decoder len =
    Decode.loop { len = len, layers = [], next = Field } tileStep


tileStep : DecoderState -> Decoder (Decode.Step DecoderState Tile)
tileStep state =
    if state.len == 0 then
        Decode.succeed (Decode.Done { layers = state.layers })

    else
        case state.next of
            Value wtype field ->
                Decode.map
                    (\( len, _ ) ->
                        Decode.Loop
                            { state
                                | next = Field
                                , len = state.len - len
                            }
                    )
                    (Proto.skip wtype)

            Field ->
                Decode.map
                    (\( len, field, wtype ) ->
                        Decode.Loop
                            { state
                                | next = Value wtype field
                                , len = state.len - len
                                , layers = { name = "layer name" } :: state.layers
                            }
                    )
                    Proto.field


type alias DecoderState =
    { layers : List Layer
    , len : Int
    , next : NextToken
    }


type NextToken
    = Field
    | Value Proto.WType Int


type alias Tile =
    { layers : List Layer
    }


type alias Layer =
    { name : String }
