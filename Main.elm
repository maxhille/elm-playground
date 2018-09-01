module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
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
    { texture : Maybe Texture
    , theta : Float
    , size : Maybe Size
    }


type alias Size =
    { w : Int
    , h : Int
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | GotViewport (Result Error Viewport)
    | Resize Int Int
    | Animate Float


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 10000 }, Cmd.none )

        Resize w h ->
            ( { model | size = Just { w = w, h = h } }, Cmd.none )

        GotViewport result ->
            case result of
                Ok viewport ->
                    update
                        (Resize
                            (round viewport.viewport.width)
                            (round viewport.viewport.height)
                        )
                        model

                Err error ->
                    ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing
      , theta = 0
      , size = Nothing
      }
    , Cmd.batch
        [ ( 53.58371, 10.05126 )
            |> tile
            |> Texture.load
            |> Task.attempt TextureLoaded
        , Task.attempt GotViewport getViewport
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


type alias LatLng =
    ( Float, Float )


tile : LatLng -> String
tile latlng =
    let
        ( x, y, z ) =
            latLngToXyzTile latlng 18
    in
    "https://stamen-tiles.a.ssl.fastly.net/watercolor/"
        ++ String.fromInt z
        ++ "/"
        ++ String.fromInt x
        ++ "/"
        ++ String.fromInt y
        ++ ".jpg"


latLngToXyzTile : LatLng -> Int -> ( Int, Int, Int )
latLngToXyzTile ( lat, lng ) zoom =
    let
        lat_rad =
            degrees lat

        n =
            toFloat (2 ^ zoom)

        xtile =
            round ((lng + 180.0) / 360.0 * n)

        ytile =
            round ((1.0 - ln (tan lat_rad + (1 / cos lat_rad)) / pi) / 2.0 * n)
    in
    ( xtile, ytile, zoom )


ln : Float -> Float
ln x =
    logBase e x


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize Resize
        , onAnimationFrameDelta Animate
        ]



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Map, yo!"
    , body =
        case model.size of
            Nothing ->
                [ text "no size yet..." ]

            Just size ->
                [ viewGl ( size, model.theta, model.texture ) ]
    }


viewGl : ( Size, Float, Maybe Texture ) -> Html Msg
viewGl ( size, theta, texture ) =
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
        (texture
            |> Maybe.map (scene (perspective theta))
            |> Maybe.withDefault []
        )


perspective : Float -> Mat4
perspective angle =
    List.foldr Mat4.mul
        Mat4.identity
        [ Mat4.makePerspective 45 1 0.01 100
        , Mat4.makeLookAt (vec3 0 3 8) (vec3 0 0 0) (vec3 0 1 0)
        , Mat4.makeRotate (3 * angle) (vec3 0 1 0)
        ]


scene : Mat4 -> Texture -> List Entity
scene camera texture =
    [ WebGL.entity
        tileVertex
        tileFragment
        (tileMesh ( -1, 0 ))
        { texture = texture
        , perspective = camera
        }
    , WebGL.entity
        tileVertex
        tileFragment
        (tileMesh ( 0, -1 ))
        { texture = texture
        , perspective = camera
        }
    , WebGL.entity
        tileVertex
        tileFragment
        (tileMesh ( 0, 0 ))
        { texture = texture
        , perspective = camera
        }
    , WebGL.entity
        tileVertex
        tileFragment
        (tileMesh ( -1, -1 ))
        { texture = texture
        , perspective = camera
        }
    ]



-- Meshes


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    , tileX : Float
    , tileZ : Float
    }


tileMesh : ( Int, Int ) -> Mesh Vertex
tileMesh ( x, z ) =
    square ( x, z )
        |> WebGL.triangles


square : ( Int, Int ) -> List ( Vertex, Vertex, Vertex )
square ( x, z ) =
    let
        nw =
            { position = vec3 0 0 0, coord = vec2 0 1, tileX = toFloat x, tileZ = toFloat z }

        ne =
            { position = vec3 1 0 0, coord = vec2 1 1, tileX = toFloat x, tileZ = toFloat z }

        sw =
            { position = vec3 0 0 1, coord = vec2 0 0, tileX = toFloat x, tileZ = toFloat z }

        se =
            { position = vec3 1 0 1, coord = vec2 1 0, tileX = toFloat x, tileZ = toFloat z }
    in
    [ ( nw, ne, sw )
    , ( sw, ne, se )
    ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    }


tileVertex : Shader Vertex Uniforms { vcoord : Vec2 }
tileVertex =
    [glsl|

        attribute vec3 position;
        attribute vec2 coord;
        attribute float tileX;
        attribute float tileZ;
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
