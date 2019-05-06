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
    { tile : Maybe Tile }


type Msg
    = GotTile (Result Http.Error Tile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTile result ->
            case result of
                Ok tile ->
                    ( { model | tile = Just tile }, Cmd.none )

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
    Decode.map (\x -> { field = Bitwise.shiftRightBy 3 x, wtype = Bitwise.and 0x07 x }) Proto.varint


type alias Tile =
    { field : Int
    , wtype : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { tile = Nothing }, loadTile )


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
                Just tile ->
                    "field: " ++ String.fromInt tile.field ++ "  type: " ++ String.fromInt tile.wtype

                Nothing ->
                    "no tile :-/"
            )
        ]
    }
