module Vector exposing (main)

import Browser
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Html exposing (Html, text)
import Http
import Proto
import Task


type alias Model =
    {}


type Msg
    = GotBytes (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


loadTile : Cmd Msg
loadTile =
    Http.get
        { url = "http://localhost:8000/tiles/14/8645/5293.pbf"
        , expect = Http.expectBytes GotBytes bytesDecoder
        }


bytesDecoder : Decoder String
bytesDecoder =
    Decode.unsignedInt32 BE
        |> Decode.andThen Decode.string


init : ( Model, Cmd Msg )
init =
    ( {}, loadTile )


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
    , body = [ text "Hello World" ]
    }
