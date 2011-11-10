(ns ggplot-clj.api
  (:use [clojure repl pprint [string :only (split)]]
        [ggplot-clj constants core]
        rosado.processing.applet
        rosado.processing)
  (:import [ggplot_clj.core Plot Vector Scale]))

(defn draw-plot
  "Draw basic background objects of the graph"
  [^Plot plot ^Scale x-axis ^Scale y-axis]
  (draw-plot-area plot)
  (draw-grid plot x-axis)
  (draw-grid plot y-axis)
  (draw-tick-marks plot x-axis)
  (draw-tick-marks plot y-axis)
  (draw-tick-labels plot x-axis)
  (draw-tick-labels plot y-axis))

(defn setup [opt]
  "Return setup function for the processing plot."
  (fn []
    (apply background-float (getopt :plot-bg-color))
    (smooth)
    (if (getopt :framerate)
      (framerate  (getopt :framerate))
      (no-loop))
    (doseq [ [f args] (getopt :init-fns)] (apply  f args))
    ))

(defn scatter-plot [x-seq y-seq & opts]
  (let [opt  (to-map opts)
        plot (Plot.
              (getopt :graph-up-left)
              (getopt :graph-size)
              (getopt :plot-margins) )
        x-axis (train-linear-scale plot x-seq :x)
        y-axis (train-linear-scale plot y-seq :y)
        draw-data (if (=  (getopt :plot-type) :points)
                    (fn [] (draw-data-points plot x-axis y-axis x-seq y-seq opts ))
                    (fn [] (draw-data-line plot x-axis y-axis x-seq y-seq opts )) ) ]
    (defapplet ggplot
      :title "ggplot"
      :setup (setup (assoc  opt :init-fns [ [draw-plot [plot x-axis y-axis]]]))
      :draw draw-data
      :size [ (:x  (getopt :graph-size)) (:y  (getopt :graph-size)) ]))
  
  (run ggplot :interactive)
  )



