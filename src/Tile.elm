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
    Decode.loop { len = len, fields = 0, next = Field } tilesStep


tilesStep : DecoderState -> Decoder (Decode.Step DecoderState Tile)
tilesStep state =
    if state.len == 0 then
        Decode.succeed (Decode.Done { fields = state.fields })

    else
        case state.next of
            Value wtype ->
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
                                | next = Value wtype
                                , len = state.len - len
                                , fields = state.fields + 1
                            }
                    )
                    Proto.field


type alias DecoderState =
    { fields : Int
    , len : Int
    , next : NextToken
    }


type NextToken
    = Field
    | Value Proto.WType


type alias Tile =
    { fields : Int
    }


type alias Layer =
    { name : String }
