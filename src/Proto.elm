module Proto exposing (parseVarint)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode exposing (..)
import Maybe


parseVarint : Bytes -> Int
parseVarint bytes =
    bytes
        |> decode varint
        |> Maybe.withDefault 0


varint : Decoder Int
varint =
    unsignedInt8
        |> andThen
            (\x ->
                if isFollowed x then
                    map (\y -> x - 0x80 + Bitwise.shiftLeftBy 7 y) unsignedInt8

                else
                    succeed x
            )


type alias VarintState =
    { isFollowed : Bool
    , sum : Int
    }



-- varintStep : VarintState -> Decoder (Step VarintState) Int
-- varintStep state =
--       if state.isFollowed then


isFollowed : Int -> Bool
isFollowed n =
    Bitwise.and n 0x80 == 0x80
