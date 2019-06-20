module Polygon2dExt exposing (WindingOrder(..), windingOrder)

import List.Extra
import Point2d exposing (xCoordinate, yCoordinate)


type WindingOrder
    = Clockwise
    | CounterClockwise
    | InvalidPolygon


windingOrder : List Point2d.Point2d -> WindingOrder
windingOrder orig =
    case orig of
        [] ->
            InvalidPolygon

        fst :: rest ->
            let
                shifted =
                    rest ++ [ fst ]

                zipped =
                    List.Extra.zip orig shifted

                shoelace =
                    List.foldl (\( p0, p1 ) a -> (xCoordinate p1 - xCoordinate p0 * yCoordinate p1 - yCoordinate p0) + a) 0 zipped
            in
            case compare shoelace 0 of
                EQ ->
                    InvalidPolygon

                GT ->
                    Clockwise

                LT ->
                    CounterClockwise
