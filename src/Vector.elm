module Vector exposing (main)

import Bitwise
import Browser
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
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
        , expect = Http.expectBytes GotTile tileDecoder
        }


tileDecoder : Decoder Tile
tileDecoder =
    Decode.loop { layers = [] } tilesStep


tilesStep : DecoderState -> Decoder (Decode.Step DecoderState Tile)
tilesStep state =
    Decode.map
        (\x ->
            case x of
                Just n ->
                    Decode.Loop { state | layers = { name = "I'm a Layer!" } :: state.layers }

                Nothing ->
                    Decode.Done state
        )
        Proto.field


type alias DecoderState =
    { layers : List Layer }


type alias Tile =
    { layers : List Layer
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
                    "layers: " ++ String.fromInt (List.length tile.layers)

                Error str ->
                    "error: " ++ str

                Idle ->
                    "idle"
            )
        ]
    }
