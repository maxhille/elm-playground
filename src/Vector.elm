module Vector exposing (main)

import Browser
import Dict
import Html exposing (Html)
import Http
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Tile exposing (..)


type alias Model =
    { tile : Data Tile.Tile }


type Data a
    = Loaded a
    | Idle
    | Error String


type Msg
    = GotTile (Result Http.Error Tile.Tile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTile result ->
            case result of
                Ok tile ->
                    ( { model | tile = Loaded tile }, Cmd.none )

                Err error ->
                    ( { model
                        | tile =
                            Error
                                (case error of
                                    Http.BadBody str ->
                                        "bad body: " ++ str

                                    _ ->
                                        "general error"
                                )
                      }
                    , Cmd.none
                    )


loadTile : Cmd Msg
loadTile =
    Http.get
        { url = "http://localhost:8000/build/tiles/14/8647/5293.pbf"
        , expect = expectProto GotTile
        }


expectProto : (Result Http.Error Tile.Tile -> msg) -> Http.Expect msg
expectProto toMsg =
    Http.expectBytesResponse toMsg <|
        resolve <|
            \bytes len ->
                Result.fromMaybe "unexpected bytes"
                    (Tile.decode len bytes)


resolve :
    (body -> Int -> Result String Tile.Tile)
    -> Http.Response body
    -> Result Http.Error Tile.Tile
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


init : ( Model, Cmd Msg )
init =
    ( { tile = Idle }, loadTile )


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }


view : Model -> Browser.Document Msg
view model =
    { title = "hello, title!"
    , body =
        [ case model.tile of
            Loaded tile ->
                viewTile tile

            Error str ->
                text <| "error: " ++ str

            Idle ->
                text "idle"
        ]
    }


viewTile : Tile -> Html Msg
viewTile tile =
    svg [ width "480", height "480", viewBox "0 0 4096 4096" ] <|
        List.concat <|
            List.map viewLayer tile.layers


viewLayer : Layer -> List (Svg Msg)
viewLayer layer =
    let
        color =
            case layer.name of
                "water" ->
                    "#3333aa22"

                "roads" ->
                    "grey"

                "building" ->
                    "brown"

                _ ->
                    "red"
    in
    List.map (viewFeature color) layer.features
        |> List.concat


viewFeature : String -> Feature -> List (Svg Msg)
viewFeature color feature =
    case feature of
        Polygons gons ->
            List.map (\gon -> viewPolygon color gon) gons

        Polyline line ->
            [ viewPolyline color line ]

        Other ->
            []


viewPolygon : String -> Polygon2d -> Svg Msg
viewPolygon color gon =
    let
        svgstr =
            gon
                |> Polygon2d.vertices
                |> List.map svgPoint
                |> String.join " "
    in
    polygon [ strokeWidth "1", fill color, stroke "black", points svgstr ] []


svgPoint : Point2d -> String
svgPoint p =
    let
        ( x, y ) =
            Point2d.coordinates p
    in
    String.fromFloat x ++ "," ++ String.fromFloat y


viewPolyline : String -> Polyline2d -> Svg Msg
viewPolyline color line =
    let
        svgstr =
            line
                |> Polyline2d.vertices
                |> List.map svgPoint
                |> String.join " "
    in
    polyline [ strokeWidth "10", fill "none", stroke color, points svgstr ] []
