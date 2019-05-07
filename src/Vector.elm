module Vector exposing (main)

import Bitwise
import Browser
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Dict
import Html exposing (Html, text)
import Http
import Proto
import Task


type alias Model =
    { tile : Data Tile }


type Data a
    = Loaded a
    | Idle
    | Error String


type Msg
    = GotTile (Result Http.Error Tile)


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


expectProto : (Result Http.Error Tile -> msg) -> Http.Expect msg
expectProto toMsg =
    Http.expectBytesResponse toMsg <|
        resolve <|
            \bytes len ->
                Result.fromMaybe "unexpected bytes"
                    (Decode.decode (tileDecoder len) bytes)


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


tileDecoder : Int -> Decoder Tile
tileDecoder len =
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
        [ text
            (case model.tile of
                Loaded tile ->
                    "layers: " ++ String.fromInt tile.fields

                Error str ->
                    "error: " ++ str

                Idle ->
                    "idle"
            )
        ]
    }
