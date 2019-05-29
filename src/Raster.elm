module Raster exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture as Texture exposing (Error, Texture)


type alias Model =
    { tiles : Tiles
    , windowSize : Maybe Size
    , location : Location
    , bearing : Float
    }


type alias Tiles =
    Dict ( Int, Int ) Tile


type alias Size =
    { w : Int
    , h : Int
    }


type alias Location =
    { lat : Float
    , lng : Float
    }


type Msg
    = TextureLoaded ( Int, Int, Int ) (Result Error Texture)
    | GotViewport (Result Error Viewport)
    | UpdateLocation Location
    | ResizeWindow Int Int
    | Animate Float


type TextureState
    = Idle
    | Updating
    | Loaded Texture


type alias Tile =
    { texture : TextureState
    , x : Int
    , y : Int
    , z : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded ( x, y, z ) textureResult ->
            case textureResult of
                Ok texture ->
                    ( { model
                        | tiles = updateTile model.tiles (Tile (Loaded texture) x y z)
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        Animate dt ->
            ( { model | bearing = model.bearing + dt / 10000 }, Cmd.none )

        ResizeWindow w h ->
            ( { model | windowSize = Just { w = w, h = h } }, Cmd.none )

        GotViewport result ->
            case result of
                Ok viewport ->
                    update
                        (ResizeWindow
                            (round viewport.viewport.width)
                            (round viewport.viewport.height)
                        )
                        model

                Err error ->
                    ( model, Cmd.none )

        UpdateLocation location ->
            let
                newTiles =
                    updateTiles model.tiles model.location

                idleTiles =
                    Dict.filter (\k tile -> tile.texture == Idle) newTiles
            in
            ( { model | location = location, tiles = newTiles }
            , loadTiles idleTiles
            )


updateTile : Tiles -> Tile -> Tiles
updateTile tiles tile =
    Dict.insert ( tile.x, tile.y ) tile tiles


updateTiles : Tiles -> Location -> Tiles
updateTiles tiles location =
    -- TODO needs diffing
    makeXyzs location
        |> List.foldr
            (\( x, y, z ) acc ->
                Dict.insert ( x, y ) (Tile Idle x y z) acc
            )
            tiles


makeXyzs : Location -> List ( Int, Int, Int )
makeXyzs location =
    let
        dist =
            20

        ( x, y, z ) =
            locationToXyz location
    in
    List.range -dist dist
        |> List.concatMap
            (\dx ->
                List.range -dist dist
                    |> List.map
                        (\dy ->
                            ( x + dx, y + dy, z )
                        )
            )


locationToXyz : Location -> ( Int, Int, Int )
locationToXyz location =
    let
        zoom =
            18

        lat_rad =
            degrees location.lat

        n =
            toFloat (2 ^ zoom)

        xtile =
            round ((location.lng + 180.0) / 360.0 * n)

        ytile =
            round ((1.0 - ln (tan lat_rad + (1 / cos lat_rad)) / pi) / 2.0 * n)
    in
    ( xtile, ytile, zoom )


ln : Float -> Float
ln x =
    logBase e x


loadTiles : Tiles -> Cmd Msg
loadTiles tiles =
    Cmd.batch (List.map loadTile (Dict.values tiles))


loadTile : Tile -> Cmd Msg
loadTile tile =
    tileUrl tile
        |> Texture.load
        |> Task.attempt (TextureLoaded ( tile.x, tile.y, tile.z ))


emptyTiles : Tiles
emptyTiles =
    Dict.empty


init : ( Model, Cmd Msg )
init =
    let
        model =
            { tiles = emptyTiles
            , windowSize = Nothing
            , bearing = 0
            , location = Location 53.58371 10.05126
            }

        ( model2, cmd ) =
            update (UpdateLocation model.location) model
    in
    ( model2
    , Cmd.batch
        [ Task.attempt GotViewport getViewport
        , cmd
        ]
    )


main : Program Value Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


tileUrl : Tile -> String
tileUrl tile =
    "https://stamen-tiles.a.ssl.fastly.net/watercolor/"
        ++ String.fromInt tile.z
        ++ "/"
        ++ String.fromInt tile.x
        ++ "/"
        ++ String.fromInt tile.y
        ++ ".jpg"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize ResizeWindow
        , onAnimationFrameDelta Animate
        ]



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Map, yo!"
    , body =
        case model.windowSize of
            Nothing ->
                [ text "no size yet..." ]

            Just size ->
                [ viewGl size model.location model.bearing model.tiles ]
    }


viewGl : Size -> Location -> Float -> Tiles -> Html Msg
viewGl size location bearing tiles =
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.stencil 0
        ]
        [ width size.w
        , height size.h
        , style "display" "block"
        ]
        (scene (perspective bearing) location tiles)


perspective : Float -> Mat4
perspective angle =
    List.foldr Mat4.mul
        Mat4.identity
        [ Mat4.makePerspective 45 1 0.01 100
        , Mat4.makeLookAt (vec3 0 3 8) (vec3 0 0 0) (vec3 0 1 0)
        , Mat4.makeRotate (3 * angle) (vec3 0 1 0)
        ]


scene : Mat4 -> Location -> Tiles -> List Entity
scene camera location tiles =
    let
        filtered =
            tiles |> Dict.values |> List.filterMap isLoaded

        center =
            locationToXyz location
    in
    List.map (makeTileEntity camera center) filtered


isLoaded : Tile -> Maybe { x : Int, y : Int, texture : Texture }
isLoaded tile =
    case tile.texture of
        Loaded texture ->
            Just { x = tile.x, y = tile.y, texture = texture }

        _ ->
            Nothing


makeTileEntity :
    Mat4
    -> ( Int, Int, Int )
    -> { x : Int, y : Int, texture : Texture }
    -> Entity
makeTileEntity camera center tile =
    let
        ( cx, cy, cz ) =
            center
    in
    WebGL.entity
        tileVertex
        tileFragment
        tileMesh
        { texture = tile.texture
        , perspective = camera
        , tileX = toFloat (tile.x - cx)
        , tileZ = toFloat (tile.y - cy)
        }



-- Meshes


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


tileMesh : Mesh Vertex
tileMesh =
    square
        |> WebGL.triangles


square : List ( Vertex, Vertex, Vertex )
square =
    let
        nw =
            { position = vec3 0 0 0, coord = vec2 0 1 }

        ne =
            { position = vec3 1 0 0, coord = vec2 1 1 }

        sw =
            { position = vec3 0 0 1, coord = vec2 0 0 }

        se =
            { position = vec3 1 0 1, coord = vec2 1 0 }
    in
    [ ( nw, ne, sw )
    , ( sw, ne, se )
    ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    , tileX : Float
    , tileZ : Float
    }


tileVertex : Shader Vertex Uniforms { vcoord : Vec2 }
tileVertex =
    [glsl|

        attribute vec3 position;
        attribute vec2 coord;
        uniform float tileX;
        uniform float tileZ;
        uniform mat4 perspective;
        varying vec2 vcoord;

        void main () {
          gl_Position = perspective * vec4(
            position.x + tileX,
            position.y,
            position.z + tileZ,
            1.0
          );
          vcoord = coord;
        }

    |]


tileFragment : Shader {} { u | texture : Texture } { vcoord : Vec2 }
tileFragment =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main () {
          gl_FragColor = texture2D(texture, vcoord);
        }

    |]
