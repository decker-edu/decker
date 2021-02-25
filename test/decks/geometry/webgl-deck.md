---
title: WebGL with shader-web-background.min.js
---


<script type="x-shader/x-fragment" id="image">
precision highp float;

uniform float iTime;

void main() {
    gl_FragColor = vec4(
        mod(gl_FragCoord.x / 256., 1.),
        mod((gl_FragCoord.x + gl_FragCoord.y - iTime * 40.) / 256. , 1.),
        mod(gl_FragCoord.y / 256., 1.),
        1.
    );
}
</script>

``` {.javascript .run}
import * as swb from "./static/shader-web-background.min.js";

console.log(swb);

let canvas = document.createElement("canvas");
canvas.setAttribute("width", "600");
canvas.setAttribute("height", "400");
anchor.appendChild(canvas);

swb.shaderWebBackground.shade({
    canvas: canvas,
    shaders: {
        image: {
            uniforms: {
                iTime: (gl, loc) => gl.uniform1f(loc, performance.now() / 1000)
            }
        }
    }
});
```
