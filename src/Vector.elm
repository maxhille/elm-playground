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
import Tile


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
        , expect = Tile.expectProto GotTile
        }


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
