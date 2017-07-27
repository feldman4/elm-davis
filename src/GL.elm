module GL exposing (..)

import Html exposing (..)
import WebGL exposing (..)
import Math.Vector3 exposing (Vec3)


main : Html msg
main =
    text ""


type alias Attributes =
    { position : Vec3
    , color : Vec3
    }


type alias VaryColor =
    { vColor : Vec3 }


renderCrap : Attributes -> Entity
renderCrap base =
    let
        -- mesh =
        --     points [ { position = vec3, color = vec3 } |> Debug.log "wtf" ]
        offset n p =
            { p | position = Math.Vector3.add p.position n }

        a =
            Math.Vector3.vec3 0.5 0 0

        b =
            Math.Vector3.vec3 0 0.5 0

        mesh =
            triangles [ ( base, base |> offset a, base |> offset b ) ]
    in
        entity dummyVertexShader dummyFragmentShader mesh ()



-- dummy shaders


dummyVertexShader : Shader Attributes () VaryColor
dummyVertexShader =
    [glsl|

precision highp float;

attribute vec3 position;
attribute vec3 color;
varying vec3 vColor;


void main () {
  vColor = color;
  // gl_Position = vec4(position, 0.0);
  gl_Position = vec4(position, 1.0);
}

|]


dummyFragmentShader : Shader {} () VaryColor
dummyFragmentShader =
    [glsl|

precision highp float;

varying vec3 vColor;

void main () {
  gl_FragColor = vec4(vColor, 0.6);
}

|]
