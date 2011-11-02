(ns ggplot-clj.core 
  (:use [clojure repl pprint]
        [rosado.processing]
        [rosado.processing.applet]
        [ggplot-clj.constants]))

(defrecord Vector [x y])

(defrecord Plot
    [anchor plot-size margins])

(def ^:dynamic *default-colors*
  {:plot-bg [255 255 255] ;(solarized-rgb :base3) 
    :plot-area-bg (solarized-rgb :base2) 
    :grid (solarized-rgb :base3)
   :tick [0 0 0]}
  )

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
  "Draws the plot area (i.e. where
  the data is plotted) of a given plot.
  This is just a colored rectangle"  
  ( [^Plot plot color]
      (draw-plot-area
       (:plot-size plot) (:margins plot) color)) 
  ([size margin color]
     (let [ [ul lr] (plot-area size margin)]
       (no-stroke)
       (apply fill-float color)
       (rect-mode CORNERS)
       (rect (:x ul) (:y ul) (:x lr) (:y lr)))))

(defn lines-along-xy 
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
  "Draws grid lines parallel to 'axis' (:x or :y) "
  [size margin ticks color wt  axis]
  (let [[ul lr] (plot-area size margin) ]
    (lines-along-xy  ticks (axis ul) (axis lr) color wt axis)))

(defn draw-tick-marks
  "Draws tick marks to 'axis' (:x or :y) "
  [size margin ticks color wt tick-length axis]
  (let [[ul lr] (plot-area size margin)
        d (/ tick-length 2)]
    (cond
     (= axis :x)
     (lines-along-xy  ticks (- (:y lr) d ) (+ (:y lr) d )  color wt :y)
     (= axis :y)
     (lines-along-xy  ticks (- (:x ul) d ) (+ (:x ul) d )  color wt :x))))

(def sample-plot
  (Plot.
   (Vector. 0 y-size)
   (Vector. x-size y-size)
   {:top 0.05 :bottom 0.075 :left 0.075 :right 0.05}   ))


;;{:length 4 :x [100 200 300] :y [150 200 250]}
;;[ [x-ticks  y-ticks]  (map :ticks (map #(% axes) [:x :y])) ]

(defn draw-ggplot []
  (draw-plot-area sample-plot (:plot-area-bg *default-colors*))
  (let [size (:plot-size sample-plot)
        margins (:margins sample-plot)
        grid-color (:grid *default-colors*)
        tick-color (:tick *default-colors*) ]
    (draw-grid size  margins [100 200] grid-color 1.5 :x)
    (draw-grid size  margins [150 200 250] grid-color 1.5 :y)
    (draw-tick-marks size  margins [100 200] tick-color 1.5 10 :y)
    (draw-tick-marks size  margins [150 200 250] tick-color 1.5 10 :x)
    ))

(defn setup []
  "Runs once."
  (apply background-float (:plot-bg *default-colors*))
  (smooth)
  (framerate 1)
  (stroke-weight 3)
  (no-loop))

(defapplet ggplot :title "ggplot"
  :setup setup :draw draw-ggplot :size [x-size y-size])

(run ggplot :interactive)

(stop ggplot)


;; =========== Testing here =======

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

(stop testbed)

