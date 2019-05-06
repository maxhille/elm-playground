module Proto exposing (varint)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode exposing (..)
import Maybe


varint : Decoder Int
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
                    }
                    varintStep
            )


type alias VarintState =
    { last : Int
    , sum : Int
    , shift : Int
    }


varintStep : VarintState -> Decoder (Step VarintState Int)
varintStep state =
    if hasMsb state.last then
        map
            (\x ->
                Loop
                    { last = x
                    , sum = state.sum + Bitwise.shiftLeftBy state.shift (Bitwise.and 0x7F x)
                    , shift = state.shift + 7
                    }
            )
            unsignedInt8

    else
        succeed (Done state.sum)


hasMsb : Int -> Bool
hasMsb n =
    Bitwise.and n 0x80 == 0x80
