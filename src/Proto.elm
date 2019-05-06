module Proto exposing (parseVarint)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode exposing (..)
import Maybe


parseVarint : Bytes -> Int
parseVarint bytes =
    bytes
        |> decode unsignedInt8
        |> Maybe.withDefault 0
