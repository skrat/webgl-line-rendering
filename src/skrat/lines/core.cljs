(ns skrat.lines.core
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :refer [vec3]]
            [thi.ng.geom.gl.webgl.constants :as GL]
            [thi.ng.geom.cuboid :as cuboid]
            [thi.ng.geom.gmesh :as gmesh]
            [thi.ng.typedarrays.core :as ta]
            [cljsjs.gl-matrix]))

(defn die [msg]
  (throw (js/Error. msg)))

(defn canvas []
  (. js/document (querySelector "canvas")))

(defn resize
  [canvas]
  (let [w 600
        h 400]
    (set! (.-width canvas) w)
    (set! (.-height canvas) h)
    [canvas w h (/ w h)]))

(defn gl-context
  [[canvas w h aspect]]
  [(. canvas (getContext "webgl")) w h aspect])

(defn clear
  [gl r g b a depth]
  (doto gl
    (.clearColor r g b a)
    (.clearDepth depth)
    (.clear (bit-or GL/color-buffer-bit
                    GL/depth-buffer-bit))))

(defn shader
  [gl typ source]
  (let [shader (.createShader gl typ)]
    (when-not shader
      (die "Could not create shader object"))
    (doto gl
      (.shaderSource shader source)
      (.compileShader shader))
    (when-not (.getShaderParameter gl shader GL/compile-status)
      (die (.getShaderInfoLog gl shader)))
    shader))

(defn program
  [gl vert frag]
  (let [program (.createProgram gl)]
    (when-not program
      (die "Could not create program object"))
    (doto gl
      (.attachShader program (shader gl GL/vertex-shader vert))
      (.attachShader program (shader gl GL/fragment-shader frag))
      (.linkProgram program))
    program))

(defn uniform
  [gl prog name]
  (let [loc (.getUniformLocation gl prog name)]
    (when-not loc
      (die (str "Location for uniform \"" name "\" not found")))
    loc))

(defn buffer
  [gl ary]
  (let [buf (.createBuffer gl)]
    (when-not buffer
      (die "Could not create buffer object"))
    (.bindBuffer gl GL/array-buffer buf)
    (.bufferData gl GL/array-buffer ary GL/static-draw)
    (.bindBuffer gl GL/array-buffer nil)
    buf))

(def vert "
  precision mediump float;
  attribute vec3 position;
  attribute vec3 other;
  attribute float dir;
  attribute vec3 color;
  uniform mat4 transform;
  uniform float thickness;
  uniform float aspect;
  varying vec3 p, pcenter;
  varying vec3 vcolor;

  void main() {
    vec2 aspect2 = vec2(aspect, 1.0);
    vec4 p0 = (transform * vec4(position, 1.0));
    vec4 p1 = (transform * vec4(other, 1.0));
    vec2 p0ndc = p0.xy / p0.w * aspect2;
    vec2 p1ndc = p1.xy / p1.w * aspect2;
    vec2 d = normalize(p1ndc - p0ndc);
    vec2 n = vec2(-d.y, d.x) * dir / aspect;

    vcolor = color;
    pcenter = p0.xyz;
    p = p0.xyz + vec3(n * thickness/2.0, 0.0);
    gl_Position = vec4(p, p0.w);
  }
")

(def frag "
  precision mediump float;
  uniform float thickness;
  varying vec3 p, pcenter;
  varying vec3 vcolor;

  void main() {
    float dist = length(p - pcenter)/(thickness/4.0);
    float a = (1.0 - pow(dist, 4.2));
    gl_FragColor = vec4(vcolor, a);
  }
")

(def subject
  (cuboid/cuboid (vec3 -0.5) (vec3 1.0)))

(def mesh
  (-> (gmesh/gmesh)
      (g/into (g/as-mesh subject))
      (g/compute-face-normals)
      (g/compute-vertex-normals)))

(defn edge-color [i]
  (if (odd? i)
    [1 0 0]
    [0 0 0]))

(def edges
  (let [left  [-1]
        right [ 1]
        index (->> (g/edges mesh) (map vec)
                   (map-indexed vector))]
    (->>
     (for [[i [v0 v1]] index
           :let [c (edge-color i)]]
       ;; position, other, dir, color
       [v0 v1 left  c
        v0 v1 right c
        v1 v0 left  c
        v1 v0 right c
        v0 v1 left  c
        v1 v0 left  c])
     (apply concat)
     (mapcat seq)
     (ta/float32))))

(def vao
  [["position" 0 3]
   ["other"    1 3]
   ["dir"      2 1]
   ["color"    3 3]])

(def vcount
  (/ (alength edges)
     (transduce (map #(nth % 2)) + vao)))

(defn vao-pointer
  [gl vao]
  (let [stride (* 4 (transduce (map #(nth % 2)) + vao))]
    (loop [[[_ i size] & vs] vao offset 0]
      (.vertexAttribPointer gl i size GL/float false stride offset)
      (when vs
        (recur vs (+ offset (* 4 size)))))))

(defn vao-bind
  [gl prog vao]
  (doseq [[name i] vao]
    (.bindAttribLocation gl prog i name)))

(defn vao-enable
  [gl vao]
  (doseq [[_ i] vao]
    (.enableVertexAttribArray gl i)))

(defn transform-model
  [m t]
  (js/mat4.identity m)
  (js/mat4.rotateX m m (* 0.0004 t))
  (js/mat4.rotateY m m (* 0.0003 t)))

(defn -main []
  (let [[gl w h aspect] (-> (canvas) resize gl-context)
        prog   (program gl vert frag)
        buffer (buffer gl edges)
        ;; --
        uaspect    (uniform gl prog "aspect")
        utransform (uniform gl prog "transform")
        uthickness (uniform gl prog "thickness")
        ;; --
        model (js/mat4.create)
        view  (-> (js/mat4.create)
                  (js/mat4.lookAt #js [0.5 1 0.5] #js [0 0 0] #js [0 0 1]))
        proj  (-> (js/mat4.create)
                  (js/mat4.perspective (* 0.55 Math/PI) aspect 0.1 100))
        mvp   (js/mat4.create)
        ;; --
        tf #(reduce
             (fn [mvp m]
               (js/mat4.mul mvp mvp m))
             (js/mat4.identity mvp)
             [proj view (transform-model model %)])
        stop (atom false)]
    (doto gl
      (.viewport 0 0 w h)
      (.bindBuffer GL/array-buffer buffer)
      (vao-pointer vao)
      (vao-bind prog vao)
      ;; --
      (.enable GL/blend)
      (.blendFuncSeparate GL/src-alpha GL/one-minus-src-alpha GL/one GL/one)
      ;; --
      (.useProgram prog)
      (vao-enable vao)
      (.uniform1f uaspect aspect)
      (.uniform1f uthickness 0.15))
    (letfn [(draw [t]
              (when-not @stop
                (clear gl 0.9 0.9 0.9 1.0 1.0)
                (.uniformMatrix4fv gl utransform false (tf t))
                (.drawArrays gl GL/triangles 0 vcount)
                (js/requestAnimationFrame draw)))]
      (draw 0))
    (fn []
      (reset! stop true)
      (js/setTimeout
       (fn []
         (.deleteBuffer gl buffer)
         (.deleteProgram gl prog)) 0))))

(defonce cleanup
  (atom nil))

(when-let [f @cleanup] (f))
(reset! cleanup (-main))
