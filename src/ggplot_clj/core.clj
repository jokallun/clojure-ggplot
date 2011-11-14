(ns ggplot-clj.core 
  (:use [clojure repl pprint [string :only (split)]]
        [rosado.processing]
        [rosado.processing.applet]
        [ggplot-clj.constants]))

(defrecord Vector [x y])

;; anchor:  location of the upper-left corner of the plot in a graph
;;plot-size: size of the plots bounding box
;; margins:  {:top :bottom :left :right} given as fraction of plto size
(defrecord
    Plot  [^Vector anchor ^Vector plot-size  margins ])

;; range:       Range of data in the source domain
;; mapping:  a function that maps a single datapoint from the
;;                       source domain to the visual property
;; type:   the type of the scale (e.g. :x :y :color etc)
(defrecord  Scale [range  mapping type])

(def ^:dynamic *default-opts*
  {:title "ggplot"
   :framerate 5
   :draw-plot-area-bg true
   :draw-grid-x true
   :draw-grid-y true
   :draw-tickmarks-x true
   :draw-tickmarks-y true
   :draw-ticklabels-x true
   :draw-ticklabels-y true
   :graph-size (Vector. 750 550)
   :graph-up-left (Vector. 0 0)
   :plot-margins {:top 0.05 :bottom 0.075 :left 0.075 :right 0.05}
   :plot-bg-color [255 255 255] ;(solarized-rgb :base3) 
   :plot-area-bg-color (solarized-rgb :base2)
   :plot-type :line
   :grid-color (solarized-rgb :base3)
   :grid-weight 1 
   :tick-color [0 0 0]
   :tick-length 10
   :point-color (solarized-rgb :base02)
   :point-size 7.5
   :line-color (solarized-rgb :base03)
   :line-size 2.5
   :font-size 15
   :line-type :plain
   :mountain-color (solarized-rgb :base01)
   :bar-color (solarized-rgb :base01)
   }
  )

;;======================
;; ==== Utils =============
(defn mapkeys
  "Apply function f to all keys in map m. m can be any sequence that
  has 2-tuples as elements (can be interpreted as map). Returns a map."
  [f m]
  (into {} (for [[k v] m] [(f  k) v]))
  )

(defn coll2map
  "Make a map from a collection, by partitioning it by 2.
  Function f is applied to the keys (element 0,2,4...  in the input collection).
  Default function f is keyword"
  ([coll] (coll2map keyword coll) )
  ([f coll]
     (cond
      (nil? coll) {}
      (and (-> coll count (= 1)) (-> coll first coll?)) (coll2map (first coll))
      (map? coll) coll
      :else ( mapkeys f (partition 2 coll)))))

(defmacro  getopt
  "Helper macro to get option 'key' from map
  called 'opt' or from '*default-opt' as fallback"
  [key]
  `(get ~'opt ~key (~key *default-opts*)))

(defn x->y [xy]
  (if (= xy :x)    :y    :x))


(defn plot-area
  "Determines what is the plot area (i.e. where
  the data is plotted) of a given plot.
  Returns vectors for upper-left and lower-right corners"
  ([^Plot plot ]
     (plot-area (:plot-size plot) (:margins plot)))
  ([size margin ]
     (let [up-left-x (* (:left margin ) (:x size))
           up-left-y (* (:top margin) (:y size))
           low-right-x (* (- 1 (:right margin)) (:x size))
           low-right-y (* (- 1 (:bottom margin)) (:y size))]
       [(Vector. up-left-x up-left-y) (Vector. low-right-x low-right-y) ])))


; ;====================
;;====Draw elements====

(defn draw-plot-area
  "Draws the plot area (i.e. where  the data is plotted)
  of a given plot.  This is just a colored rectangle"  
  [^Plot plot & opts]
  (let [opt (coll2map opts) 
        [up-left low-right] (plot-area plot)
        color (getopt :plot-area-bg-color)]
    (no-stroke)
    (apply fill-float color)
    (rect-mode CORNERS)
    (rect (:x up-left) (:y up-left) (:x low-right) (:y low-right)))
  )

(defn- lines-along-xy 
  "Draws lines parallel to  :x or :y axis. Lines are drawn at locations
  in sequence v and have a length determined by start/end parameters.
  'color' is the line color and 'wt' the line width"
  [v start end color wt axis]
  (apply stroke-float color)
  (stroke-weight wt)
  (doseq [e v]
    (apply line
           (cond
            (= axis :x) (interleave [start end] [e e])
            (= axis :y) (interleave  [e e] [start end])))))

(defn draw-grid
  "Draws grid lines for  'axis' "
  [^Plot plot ^Scale axis & opts]
  (let [opt (coll2map opts)
            [up-left low-right] (plot-area plot)
            direction (x->y (:type axis))
            color (getopt :grid-color)
            wt (getopt :grid-weight)]
        (lines-along-xy (map (:mapping axis) (:range axis))
                        (direction up-left) (direction low-right)
                        color wt direction)))

(defn draw-tick-marks
  "Draws tick marks to 'axis'"
  [^Plot plot ^Scale axis & opts]
  (let [opt (coll2map opts) 
        [up-left low-right] (plot-area plot)
        half (/ (getopt :tick-length) 2)
        color (getopt :tick-color)
        ticks (map (:mapping axis) (:range axis))
        wt (getopt :grid-weight)]
    (cond
     (= (:type  axis) :x) (lines-along-xy
                           ticks (- (:y low-right) half ) (+ (:y low-right) half )  color wt :y)
     (= (:type  axis) :y) (lines-along-xy
                           ticks (- (:x up-left) half ) (+ (:x up-left) half )  color wt :x))))

(defn draw-tick-labels
  "Writes positions of tick marks to 'axis' "
  [^Plot plot ^Scale axis & opts]
  (let [opt (coll2map opts)
        ticks (map  (:mapping axis) (:range axis))
        [up-left low-right] (plot-area plot)] 
       (fill-int 0)
       (cond
        (= (:type  axis) :x) (do
                               (text-align CENTER TOP)
                               (doseq
                                      [x (:range axis)]
                                 (string->text (str x)  ;; text
                                                  ((:mapping axis) x) ;; x-coord
                                                  (+ (:y low-right) (getopt :tick-length))))) ;;ycoord
        (= (:type  axis) :y) (do
                               (text-align RIGHT CENTER)
                               (doseq
                                      [y (:range axis)]
                                    (string->text (str y)  ;; text
                                                  (- (:x up-left) (getopt :tick-length))
                                                  ((:mapping axis) y)))) )))

(defn draw-bar
  "Primitive for drawing bar-charts"
  [^Plot plot  pos height width & opts]
  (rect-mode CORNERS)
  (no-stroke)
  ;;(pprint width)
  ;;(pprint pos)
  ;;(pprint height)
  (let [[up-left low-right] (plot-area plot)
        w2 (/ width 2)
        opt (coll2map opts)]
    (apply fill-float (getopt :bar-color))
    (rect (- pos w2)  (:y low-right) (+ pos w2) (- (:y low-right) height) )))


;;============================
;; ====== Draw data ============

(defn draw-data-bars
  "Bar chart"
  [^Plot plot ^Scale label-axis ^Scale value-axis labels values & opts]
  (let [opt (coll2map opts)
        pos (map (:mapping label-axis) labels)
        height (map (:mapping value-axis) values)
        plot-data (interleave pos height)
        width (* 0.85 (apply - (take 2 pos)))
        ]
    (doseq [ [p h] (partition 2 plot-data) ]
      (draw-bar plot p h width opt ))))

(defn draw-data-points-simple
  "Draw data points with a constant color and size"
  [^Scale x-axis ^Scale y-axis x-data y-data & opts]
  (let [opt (coll2map opts) 
         point-seq (partition 2 (interleave x-data y-data))
         xmap (:mapping x-axis)
         ymap (:mapping y-axis)
         color (getopt :point-color)
        point-size ( getopt :point-size )] 
    (ellipse-mode CENTER)
    (no-stroke)
    (apply fill-float color)    
    (doseq [p point-seq]
      (ellipse
       (xmap  (first p)) (ymap (second p)) point-size point-size))))

(defn draw-data-points
  "Draw data points with a variable color and/or  size"
  [^Scale x-axis ^Scale y-axis  ^Scale color ^Scale size
   x-data y-data color-data size-data & opts]
  ( let [opt (coll2map opts)
         c-data (or color-data
                    (repeat (count x-data) (getopt :point-color)) )
         s-data (or  size-data
                     (repeat (count x-data) (getopt :point-size)) )
         point-seq (partition 4 (interleave x-data  y-data c-data size-data))
         xmap (:mapping x-axis)
         ymap (:mapping y-axis)
         colormap (if color (:mapping color) (fn [x] x))
         sizemap (if size  (:mapping size) (fn [x] x))] 
    (ellipse-mode CENTER)
    (no-stroke)
    (doseq [[x y c s] point-seq]
      (apply fill-float (colormap  c))    
      (ellipse
       (xmap  x) (ymap y) (sizemap s) (sizemap s))))  )
 
(defn draw-data-line
  [^Scale x-axis ^Scale y-axis x-data y-data & opts]
  ( let [opt (coll2map opts)  
         point-seq (partition 2 (interleave x-data y-data))
         color (getopt :line-color)
         line-size  (getopt :line-size)
         xmap (:mapping x-axis)
         ymap (:mapping y-axis)]
    (no-fill)
    (apply stroke-float color)
    (stroke-weight line-size)
    (begin-shape)
    (let [ [px py] (first point-seq)]
      (curve-vertex (xmap px) (ymap py)))
    (doseq [[px py] point-seq] 
      (curve-vertex 
       (xmap  px) (ymap py)))
    (let [ [px py] (first (reverse  point-seq))]
      (curve-vertex (xmap px) (ymap py)))
    (end-shape))
  )

(defn draw-data-mountain
  [^Plot plot ^Scale x-axis ^Scale y-axis x-data y-data & opts]
  ( let [opt (coll2map opts) 
         point-seq (partition 2 (interleave x-data y-data))
         color (getopt :mountain-color)
         xmap (:mapping x-axis)
         ymap (:mapping y-axis)
         [up-left low-right] (plot-area plot)]
    (apply fill-float (getopt :mountain-color))
    ;;(stroke-int 0 )
    (no-stroke)
    (begin-shape)
    (let [ [px py] (first point-seq)]
      (vertex (xmap px) (:y low-right))
      (vertex (xmap px) (ymap py)))
    (doseq [[px py] point-seq] 
      (curve-vertex 
       (xmap  px) (ymap py)))
    (let [ [px py] (first (reverse  point-seq))]
      (vertex (xmap px) (ymap py))
      (vertex (xmap px) (:y low-right)))
    (end-shape CLOSE)
    (draw-grid plot x-axis :grid-color [255 255 255]))
  )

;; ==== Fitting scales =====
;;=====================

(defn- natural-scale [d]
  (let [ [fc ex] (map read-string (split (format "%E" d) #"E"))
         sc (Math/pow  10  ex)]
    (loop [tick sc]
      (if (> d (* 4.1 tick))
        tick
        (recur (/ tick 2)))) ))

(defn get-range 
  "Fit the range of a linear axis and its tick marks
  to the data in seq"
  [seq]
  (let [mx (reduce max seq)
        mn (reduce min seq)
        tick-size (natural-scale (double  (- mx mn)))]
    (range
     (* tick-size  (Math/floor (/ mn tick-size)))
     (* tick-size  (inc  (Math/ceil (/ mx tick-size))))
     tick-size)))

(defmulti train-mapping
  "Create a function to map from the data to the
  visual property. "
  (fn [scale-type source-range target-range & opts] scale-type))

(defmethod train-mapping :x [ _ source-range target-range & opts]
  (let [source-mx (first (reverse source-range)) 
        source-mn (first source-range)  
        target-mx (first (reverse target-range)) 
        target-mn (first target-range)  
        coeff (/
               (- target-mx target-mn)
               (- source-mx source-mn)) ]
    (fn [x] (+ target-mn  (* (- x source-mn) coeff)))
   ))

(defmethod train-mapping :y [ _ source-range target-range & opts]
  ;; processing defines the y coordinate as distance from upper limit
  ;; downwards and therefore the target range must be defined
  ;; in opposite order
  (train-mapping :x source-range (reverse target-range)))

(defn train-linear-scale
  "Get a linear scale. Direction must be :x or :y"
  [^Plot plot seq direction]
  (let [range (get-range seq)
        plot-range (map direction (plot-area plot))
        mapping (train-mapping direction range plot-range)]
    (Scale. range mapping direction)))


(defn train-discrete-scale
  "Divide an axis into equally long parts"
  [^Plot plot labels direction]
  (let  [ [up-left low-right] (plot-area plot)
          valcnt (count labels)
          axlen (- (:x low-right) (:x up-left))
          interval (/ axlen valcnt)
          pos (range (+ (:x up-left) (/ interval 2)) (:x low-right) interval)
          mapping (apply hash-map (interleave labels pos))] 
    ;; plot-data (interleave pos (map (:mapping value-axis) values))
    (Scale. labels (fn [key] (get mapping key)) direction)))

