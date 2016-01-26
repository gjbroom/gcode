(ns gcode.core)

(defn init []
  (vector {:x 0.0 :y 0.0 :z 0.0
           :a 0.0 :b 0.0 :c 0.0
           :u 0.0 :v 0.0 :w 0.0
           :motion nil
           :units :mm
           :distance :absolute
           :arcDistance :relative
           :plane :xy
           :lathe :dia
           :feed :perMinute
           :spindle :disabled
           :coolant false
           :tool 0
           :feedRate 0.0
           
           :nc "G21G90"
           }))

(defmacro maybe-param [curr k x]
  `(if ~x
     (if (= (~curr :distance) :absolute)
       ~x
       (+ (~curr ~k) ~x))
     (~curr ~k)))

(defn motion-seek [state-vec & {:keys [x y z a b c u v w]}]
  (let [curr (last state-vec)
        abs? (= (curr :distance) :absolute)
        repr (str "G0"
                  (if x (str "X" x) "")
                  (if y (str "Y" y) "")
                  (if z (str "Z" z) "")
                  (if a (str "A" a) "")
                  (if b (str "B" b) "")
                  (if c (str "C" c) "")
                  (if u (str "U" u) "")
                  (if v (str "V" v) "")
                  (if w (str "W" w) ""))
        ]
    (conj state-vec (assoc curr
                           :x (maybe-param curr :x x)
                           :y (maybe-param curr :y y)
                           :z (maybe-param curr :z z)
                           :a (maybe-param curr :a a)
                           :b (maybe-param curr :b b)
                           :c (maybe-param curr :c c)
                           :u (maybe-param curr :u u)
                           :v (maybe-param curr :v v)
                           :w (maybe-param curr :w w)
                           :motion :seek :nc repr))))
(defn motion-linear [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion :linear :nc "G1"))))
(defn motion-cw-arc [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion :cw :nc "G2"))))
(defn motion-ccw-arc [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion :ccw :nc "G3"))))
(defn motion-none [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion nil :nc "G80"))))
(defn motion-cubic-spline [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion :cSpline :nc "G5"))))
(defn motion-quadratic-spline [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion :qSpline :nc "G5.1"))))
(defn motion-nurbs-open [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion :nurbs :nc "G5.2"))))
(defn motion-nurbs-close [state-vec & args]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :motion nil :nc "G5.3"))))

(defn lathe-diameter [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :lathe :dia :nc "G7"))))
(defn lathe-radius [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :lathe :rad :nc "G8"))))

(defn units-mm [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :units :mm :nc "G21"))))
(defn units-in [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :units :inch :nc "G20"))))

(defn distance-abs [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :distance :absolute :nc "G90"))))
(defn distance-rel [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :distance :relative :nc "G91"))))
(defn distance-arc-abs [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :arcDistance :absolute :nc "G90.1"))))
(defn distance-arc-rel [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :arcDistance :rel :nc "G91.1"))))

(defn plane-xy [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :plane :xy :nc "G17"))))
(defn plane-zx [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :plane :zx :nc "G18"))))
(defn plane-yz [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :plane :yz :nc "G19"))))
(defn plane-uv [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :plane :uv :nc "G17.1"))))
(defn plane-wu [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :plane :wu :nc "G18.1"))))
(defn plane-vw [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :plane :vw :nc "G19.1"))))

(defn feed-rate-inv-time [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :feed :inv :nc "G93"))))
(defn feed-rate-per-minute [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :feed :perMinute :nc "G94"))))
(defn feed-rate-per-revolution [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :feed :perRevolution :nc "G95"))))

(defn spindle-cw-enable [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :spindle :cw :nc "M3"))))
(defn spindle-ccw-enable [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :spindle :ccw :nc "M4"))))
(defn spindle-disable [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :spindle :disabled :nc "M5"))))

(defn coolant-mist-enable [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :coolant true :nc "M7"))))
(defn coolant-flood-enable [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :coolant true :nc "M8"))))
(defn coolant-disable [state-vec]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :coolant false :nc "M9"))))

(defn tool-select [state-vec tool]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :tool tool :nc (str "T" tool)))))

(defn feed-rate-set [state-vec rate]
  (let [curr (last state-vec)]
    (conj state-vec (assoc curr :feedRate rate :nc (str "F" rate)))))

(defn code [state-vec]
  (map :nc state-vec))
