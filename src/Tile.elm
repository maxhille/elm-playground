module Tile exposing (Command(..), Feature, GeomType(..), Layer, Tile, decode)

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
    Decode.decode (tileDecoder len) bs


tileDecoder : Int -> Decoder Tile
tileDecoder len =
    Decode.loop { len = len, layers = [] } tileStep


tileStep : TileDecoderState -> Decoder (Decode.Step TileDecoderState Tile)
tileStep state =
    if state.len == 0 then
        Decode.succeed (Decode.Done { layers = state.layers })

    else
        Proto.decodeKey
            |> Decode.andThen
                (\( klen, k, wtype ) ->
                    case k of
                        3 ->
                            Decode.map
                                (\( vlen, layer ) ->
                                    Decode.Loop
                                        { state
                                            | len = state.len - klen - vlen
                                            , layers = layer :: state.layers
                                        }
                                )
                                (Proto.varint
                                    |> Decode.andThen
                                        (\( len, x ) ->
                                            Decode.map
                                                (\layer -> ( len + x, layer ))
                                                (Decode.loop
                                                    { len = x
                                                    , name = ""
                                                    , features = []
                                                    }
                                                    layerStep
                                                )
                                        )
                                )

                        _ ->
                            Decode.map
                                (\( vlen, _ ) ->
                                    Decode.Loop
                                        { state | len = state.len - klen - vlen }
                                )
                                (Proto.decodeBytes wtype)
                )


layerStep : LayerDecoderState -> Decoder (Decode.Step LayerDecoderState Layer)
layerStep state =
    if state.len == 0 then
        Decode.succeed
            (Decode.Done
                { name = state.name
                , features = state.features
                }
            )

    else
        Proto.decodeKey
            |> Decode.andThen
                (\( klen, k, wtype ) ->
                    case k of
                        1 ->
                            Decode.map
                                (\( vlen, str ) ->
                                    Decode.Loop
                                        { state
                                            | len = state.len - klen - vlen
                                            , name = str
                                        }
                                )
                                Proto.decodeString

                        2 ->
                            Decode.map
                                (\( vlen, feature ) ->
                                    Decode.Loop
                                        { state
                                            | len = state.len - klen - vlen
                                            , features = feature :: state.features
                                        }
                                )
                                (Proto.varint
                                    |> Decode.andThen
                                        (\( len, x ) ->
                                            Decode.map
                                                (\layer -> ( len + x, layer ))
                                                (Decode.loop
                                                    { len = x
                                                    , geomType = Unknown
                                                    , geometry = []
                                                    }
                                                    featureStep
                                                )
                                        )
                                )

                        _ ->
                            Decode.map
                                (\( vlen, _ ) ->
                                    Decode.Loop
                                        { state | len = state.len - klen - vlen }
                                )
                                (Proto.decodeBytes wtype)
                )


featureStep : FeatureDecoderState -> Decoder (Decode.Step FeatureDecoderState Feature)
featureStep state =
    if state.len == 0 then
        Decode.succeed
            (Decode.Done
                { geometry = state.geometry
                , geomType = state.geomType
                }
            )

    else
        Proto.decodeKey
            |> Decode.andThen
                (\( klen, k, wtype ) ->
                    case k of
                        4 ->
                            Decode.map
                                (\( vlen, xs ) ->
                                    Decode.Loop
                                        { state
                                            | len = state.len - klen - vlen
                                            , geometry = xs
                                        }
                                )
                                decodeGeometry

                        3 ->
                            Decode.map
                                (\( vlen, x ) ->
                                    Decode.Loop
                                        { state
                                            | len = state.len - klen - vlen
                                            , geomType = geomType x
                                        }
                                )
                                Proto.varint

                        _ ->
                            Decode.map
                                (\( vlen, _ ) ->
                                    Decode.Loop
                                        { state | len = state.len - klen - vlen }
                                )
                                (Proto.decodeBytes wtype)
                )


decodeGeometry : Decoder ( Int, List Command )
decodeGeometry =
    Proto.varint
        |> Decode.andThen
            (\( len, x ) ->
                Decode.map (\xs -> ( x + len, xs )) (Decode.loop ( x, [] ) geometryStep)
            )


geometryStep : GeometryDecoderState -> Decoder (Decode.Step GeometryDecoderState (List Command))
geometryStep ( len, cs ) =
    if len == 0 then
        Decode.succeed (Decode.Done cs)

    else
        Proto.varint
            |> Decode.andThen
                (\( len2, x ) ->
                    case Bitwise.and x 0x07 of
                        1 ->
                            let
                                n =
                                    Bitwise.shiftRightBy 3 x
                            in
                            Decode.map
                                (\( len3, xs ) ->
                                    Decode.Loop ( len - len2 - len3, List.append cs (List.map (\( y, z ) -> MoveTo y z) xs) )
                                )
                                (Decode.loop ( n, 0, [] ) repeatStep)

                        2 ->
                            let
                                n =
                                    Bitwise.shiftRightBy 3 x
                            in
                            Decode.map
                                (\( len3, xs ) ->
                                    Decode.Loop ( len - len2 - len3, List.append cs (List.map (\( y, z ) -> LineTo y z) xs) )
                                )
                                (Decode.loop ( n, 0, [] ) repeatStep)

                        7 ->
                            Decode.succeed <| Decode.Loop ( len - len2, List.append cs [ ClosePath ] )

                        _ ->
                            Decode.fail
                )


repeatStep :
    ( Int, Int, List ( Int, Int ) )
    -> Decoder (Decode.Step ( Int, Int, List ( Int, Int ) ) ( Int, List ( Int, Int ) ))
repeatStep ( n, len, xs ) =
    if n == 0 then
        Decode.succeed (Decode.Done ( len, xs ))

    else
        Decode.map2
            (\( len1, y ) ( len2, z ) ->
                Decode.Loop ( n - 1, len + len2 + len1, List.append xs [ ( y, z ) ] )
            )
            Proto.sint32
            Proto.sint32


type alias LayerDecoderState =
    { name : String
    , features : List Feature
    , len : Int
    }


type alias FeatureDecoderState =
    { geometry : List Command
    , geomType : GeomType
    , len : Int
    }


type alias TileDecoderState =
    { layers : List Layer
    , len : Int
    }


type alias GeometryDecoderState =
    ( Int, List Command )


type Command
    = MoveTo Int Int
    | LineTo Int Int
    | ClosePath


type NextToken
    = Field
    | Value Proto.WType Int


type alias Tile =
    { layers : List Layer
    }


type alias Feature =
    { geomType : GeomType
    , geometry : List Command
    }


type alias Layer =
    { name : String
    , features : List Feature
    }


type GeomType
    = Unknown
    | Point
    | LineString
    | Polygon


geomType : Int -> GeomType
geomType x =
    case x of
        1 ->
            Point

        2 ->
            LineString

        3 ->
            Polygon

        _ ->
            Unknown
