module Vector exposing (main)

import Browser
import Dict
import Html exposing (Html, text)
import Http
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
        { url = "http://localhost:8000/tiles/14/8645/5293.pbf"
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
    Html.ul [] <|
        List.map viewLayer tile.layers


viewLayer : Layer -> Html Msg
viewLayer layer =
    Html.li []
        [ text layer.name
        , Html.ul [] <| List.map viewFeature layer.features
        ]


viewFeature : Feature -> Html Msg
viewFeature feature =
    Html.li []
        [ text <|
            case feature.geomType of
                Polygon ->
                    "Polygon"

                LineString ->
                    "LineString"

                _ ->
                    "Unhandled"
        , Html.ul [] <| List.map viewCommand feature.geometry
        ]


viewCommand : Command -> Html Msg
viewCommand cmd =
    Html.li []
        [ text <|
            case cmd of
                MoveTo x y ->
                    "MoveTo"

                LineTo x y ->
                    "LineTo"

                ClosePath ->
                    "ClosePath"
        ]
