(ns ggplot-clj.core 
  (:use [clojure repl pprint [string :only (split)]]
        [rosado.processing]
        [rosado.processing.applet]
        [ggplot-clj.constants]
        [clojure-utils.collections]))

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
  {:framerate 5
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
   }
  )

;;======================
;; ==== Drawing elements ===

(defmacro  getopt
  "Helper macro to get option 'key' from map
  called 'opt' or from '*default-opt' as fallback"
  [key]
  `(get ~'opt ~key (~key *default-opts*)))

(defn x->y [xy]
  (if (= xy :x)    :y    :x))

;; (defn to-map [opts]
;;   (cond
;;    (map? opts) opts
;;    (nil? opts) {}   
;;       (apply assoc  {} (or  opts [nil nil]))))

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
 
(defn draw-plot-area
  "Draws the plot area (i.e. where  the data is plotted)
  of a given plot.  This is just a colored rectangle"  
  [^Plot plot & opts]
  (let [opt (coll2map opts) 
        [up-left low-right] (plot-area plot)
        color (getopt :plot-area-bg-color)]
    (pprint color)
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

;; ====== Draw data ============
;;============================

(defn draw-data-points
  [^Plot plot ^Scale x-axis ^Scale y-axis x-data y-data & opts]
  ( let [opt (coll2map opts) 
         point-seq (partition 2 (interleave x-data y-data))
         color (getopt :point-color)
         point-size  (getopt :point-size)
         xmap (:mapping x-axis)
         ymap (:mapping y-axis)]
    (ellipse-mode CENTER)
    (no-stroke)
    (apply fill-float color)    
    (doseq [p point-seq]
      (ellipse
       (xmap  (first p)) (ymap (second p)) point-size point-size)))
  )
 
(defn draw-data-line
  [^Plot plot ^Scale x-axis ^Scale y-axis x-data y-data & opts]
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

;; =========== Testing here =======
(comment 

  (def myseq (repeatedly 20  #(-  (rand-int 1000) 300)))



  
  

  (defn test-setup []
    (smooth)
    (framerate 5)
    (stroke-weight 1)
    (no-loop))

  (defn test-draw []
    (text-align LEFT TOP)
    (text-size 25)
    (let [kk (sort (keys solarized))
          nk (count kk)
          ypos (map #(* (/ y-size nk) %) (range nk))
          el (partition 2 (interleave kk ypos))]
      (doseq [ [clr y] el]
        (apply fill-float (solarized-rgb clr))
        (rect 0  y x-size (+ y (/ y-size nk)))
        (fill-int 0)
        (string->text  (str clr) 20 y)
        )
      ))

  (defapplet testbed :title "testbed"
    :setup test-setup :draw test-draw :size [x-size y-size])

  (run testbed :interactive)

  (stop testbed))

