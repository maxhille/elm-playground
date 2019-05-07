module Proto exposing (Field, WType(..), field, skip, varint)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode exposing (..)
import Maybe


varint : Decoder ( Int, Int )
varint =
    unsignedInt8
        |> andThen
            (\x ->
                loop
                    { last = x
                    , sum =
                        if hasMsb x then
                            x - 0x80

                        else
                            x
                    , shift = 7
                    , len = 1
                    }
                    varintStep
            )


type alias VarintState =
    { last : Int
    , len : Int
    , sum : Int
    , shift : Int
    }


varintStep : VarintState -> Decoder (Step VarintState ( Int, Int ))
varintStep state =
    if hasMsb state.last then
        map
            (\x ->
                Loop
                    { last = x
                    , sum = state.sum + Bitwise.shiftLeftBy state.shift (Bitwise.and 0x7F x)
                    , shift = state.shift + 7
                    , len = state.len + 1
                    }
            )
            unsignedInt8

    else
        succeed (Done ( state.len, state.sum ))


hasMsb : Int -> Bool
hasMsb n =
    Bitwise.and n 0x80 == 0x80


type alias Field =
    ( Int, Int, WType )


type WType
    = Varint
    | Bit64
    | Delim
    | Bit32
    | Unsupported


wtype : Int -> WType
wtype x =
    case x of
        0 ->
            Varint

        1 ->
            Bit64

        2 ->
            Delim

        5 ->
            Bit32

        _ ->
            Unsupported


field : Decoder Field
field =
    map (\( len, x ) -> ( len, Bitwise.shiftRightBy 3 x, wtype <| Bitwise.and 0x07 x )) varint


skip : WType -> Decoder ( Int, Int )
skip wt =
    case wt of
        Varint ->
            varint

        Bit64 ->
            map (\bs -> ( 8, Bytes.width bs )) (bytes 8)

        Delim ->
            varint
                |> andThen (\( len, x ) -> map (\y -> ( len, y )) (bytes x))
                |> map (\( len, bs ) -> ( len + Bytes.width bs, 0 ))

        Bit32 ->
            map (\bs -> ( 8, Bytes.width bs )) (bytes 4)

        Unsupported ->
            fail
