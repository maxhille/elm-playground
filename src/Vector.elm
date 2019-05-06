module Vector exposing (main)

import Browser
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Html exposing (Html, text)
import Http
import Proto
import Task


type alias Model =
    { x : Int }


type Msg
    = GotTile (Result Http.Error Tile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTile result ->
            case result of
                Ok tile ->
                    ( { model | x = tile.x }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


loadTile : Cmd Msg
loadTile =
    Http.get
        { url = "http://localhost:8000/tiles/14/8645/5293.pbf"
        , expect = Http.expectBytes GotTile tileDecoder
        }


tileDecoder : Decoder Tile
tileDecoder =
    Decode.map (\x -> { x = x }) Decode.unsignedInt8


type alias Tile =
    { x : Int }


init : ( Model, Cmd Msg )
init =
    ( { x = 0 }, loadTile )


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
    , body = [ text ("Hello " ++ String.fromInt model.x) ]
    }
