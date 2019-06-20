module Polygon2dExtTests exposing (suite)

import Expect
import Point2d
import Polygon2dExt exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Polygon2dExt module"
        [ describe "windingOrder"
            [ test "counter-clockwise" <|
                -- example from https://stackoverflow.com/a/1165943
                \_ ->
                    let
                        ps =
                            [ Point2d.fromCoordinates ( 5, 0 )
                            , Point2d.fromCoordinates ( 6, 4 )
                            , Point2d.fromCoordinates ( 4, 5 )
                            , Point2d.fromCoordinates ( 1, 5 )
                            , Point2d.fromCoordinates ( 1, 0 )
                            ]
                    in
                    Expect.equal CounterClockwise (windingOrder ps)
            ]
        ]
