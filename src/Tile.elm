module Tile exposing (Tile, expectProto)

import Bitwise
import Browser
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Dict
import Html exposing (Html, text)
import Http
import Proto
import Task


expectProto : (Result Http.Error Tile -> msg) -> Http.Expect msg
expectProto toMsg =
    Http.expectBytesResponse toMsg <|
        resolve <|
            \bytes len ->
                Result.fromMaybe "unexpected bytes"
                    (Decode.decode (decoder len) bytes)


resolve :
    (body -> Int -> Result String Tile)
    -> Http.Response body
    -> Result Http.Error Tile
resolve toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            let
                contentLength =
                    Dict.get "content-length" metadata.headers
                        |> Maybe.andThen String.toInt
            in
            case contentLength of
                Just len ->
                    Result.mapError Http.BadBody (toResult body len)

                Nothing ->
                    -- TODO custom errors
                    Err Http.NetworkError


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
