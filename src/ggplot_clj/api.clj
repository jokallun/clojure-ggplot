(ns ggplot-clj.api
  (:use [clojure repl pprint [string :only (split)]]
        [ggplot-clj constants core]
        rosado.processing.applet
        rosado.processing)
  (:import [ggplot_clj.core Plot Vector Scale]))


(defn draw-plot-bg
  "Draw basic background elements of the plot
  Valid opitions and their defaults (all booleans):
    :draw-plot-area-bg true
   :draw-grid-x true
   :draw-grid-y true
   :draw-tickmarks-x true
   :draw-tickmarks-y true
   :draw-ticklabels-x true
   :draw-ticklabels-y true "
  [^Plot plot ^Scale x-axis ^Scale y-axis & opts]
  (let [opt (coll2map opts)]
    (when (getopt :draw-plot-area-bg) (draw-plot-area plot))
    (when (getopt :draw-grid-x) (draw-grid plot x-axis))
    (when (getopt :draw-grid-y) (draw-grid plot y-axis))
    (when (getopt :draw-tickmarks-x)(draw-tick-marks plot x-axis))
    (when (getopt :draw-tickmarks-y)(draw-tick-marks plot y-axis))
    (when (getopt :draw-ticklabels-x)(draw-tick-labels plot x-axis))
    (when (getopt :draw-ticklabels-y)(draw-tick-labels plot y-axis))))

(defn setup
  "Return setup function for the processing plot.
   Valid options:
    :init-fns -a list of functions with
       their arguments to be run at the setup. Value of :init-fns
      must be a seq of pairs (function, its arguments)
   :plot-bg-color -rgb values, a seq of length 3
   :framerate -plot update frequency. nil means static plot"
  [opt]
  (fn []
    (apply background-float (getopt :plot-bg-color))
    (smooth)
    (if (getopt :framerate)
      (framerate  (getopt :framerate))
      (no-loop))
    (doseq [ [f args] (partition 2 (getopt :init-fns))] (apply  f args))
    ))


(defn draw-data
  "A wrapper to run all listed functions on each update
  of the ggplot applet.
  draw-fns is a seq of pairs (function, its arguments) which are the
  executed at each update"
  [draw-fns]
  (fn []
    (doseq [[f args] (partition 2  draw-fns)]  (apply f args) )))

(defn new-plot
  "create a new plot"
  [& opts]
  (let [opt (coll2map opts)]
    (Plot.
     (getopt :graph-up-left)
     (getopt :graph-size)
     (getopt :plot-margins))))


;; data:  a map {variable name, values} all values should be
;;              sequences of equal length
;; aes:  binds a variable to a visual propery (aestetihc) through a scale
;; geom:  geometrical object (point, line, bar...)
;; stat: transformation to apply (binning, average...)
(defrecord Layer[data aes geom stat position options])

(defn  geom-point
  "Convenience function to create a layer for point geoms"
  [ & { :keys [data aes stat position]
       :or {data {} aes {} stat :identity position :fix}
       :as args}]
  (pprint data)
  (Layer. data aes :point stat position (dissoc args :data :aes :stat :position )) )

(defn add-data
  "Add data to a layer, for example:
   (add-data a-lyr :height [1 3 2] :weight [57 345 55])
  adds two new variables to the layers data"
  [^Layer layer & data]
  (assoc layer :data
         (merge (:data layer)  (coll2map data))))

(defn get-data
  "get the data  for aestathic properties listed in vars
  (e.g. :x :y :color ..). aes provides the mapping to
  correct variables, which in turn are stored in data."
  [ data aes & vars ]
  (let [var-names (map #(get aes) vars)]
    (map #(get data % nil) var-names )))

(defmulti get-plot-fns
  "Get the drawing functions for a given geom"
  (fn [geom aes position data scales & opts ] geom))

(defmethod get-plot-fns :point
  [_ aes position data scales & opts]
  (let [  dt (get-data data aes :x :y :color :size)
        scl (map #(get scales % nil) [:x :y :color :size])]
    (fn [] (apply draw-data-points (concat scl dt opts)))))

(defn make-plot [ & { :keys [ layers scales data] :as opts } ]
  (let [opt  (dissoc opts :layers :scales :data)
        all-data (reduce merge data (map :data layers))]
    (for [layer layers]
      (get-plot-fns (:geom layer) (:aes layer) (:position layer) all-data scales opt))))

(defn scatter-plot [x-seq y-seq & opts]
  (let [opt  (coll2map opts)
        x-axis (train-linear-scale plot x-seq :x)
        y-axis (train-linear-scale plot y-seq :y)
        layer (geom-point
               :data {:var1 x-seq :var2 y-seq}
               :aes {:x :var1 :y :var2})
        plot-fns (make-plot
                  :layers [layer]
                  :scales {:x x-axis :y y-axis} ) ]
    (defapplet ggplot
      :title (getopt :title)
      :setup (setup (assoc opt :init-fns [draw-plot-bg [plot x-axis y-axis]]))
      :draw (fn [] (doseq [f plot-fns] (f)))
      :size [ (:x  (getopt :graph-size)) (:y  (getopt :graph-size)) ]))  
  ;; run applet
  (run ggplot :interactive)
  )


(defn scatter-plot [x-seq y-seq & opts]
  (let [opt  (coll2map opts)
        plot (new-plot opt)
        x-axis (train-linear-scale plot x-seq :x)
        y-axis (train-linear-scale plot y-seq :y)
        pl-type (getopt :plot-type)
        args [x-axis y-axis x-seq y-seq opt]
        draw-fns (cond
                  (= :points  pl-type) [draw-data-points-simple  args]
                  (= :line pl-type) [draw-data-line args]
                  (= :both  pl-type) [draw-data-points-simpe  args draw-data-line args]) ]
    (defapplet ggplot
      :title (getopt :title)
      :setup (setup (assoc opt :init-fns [draw-plot-bg [plot x-axis y-axis]]))
      :draw (draw-data (concat  [draw-plot-bg [plot x-axis y-axis]] draw-fns))
      :size [ (:x  (getopt :graph-size)) (:y  (getopt :graph-size)) ]))  
  ;; run applet
  (run ggplot :interactive)
   )

(defn bar-plot [labels values & opts]
  (let [opt (coll2map opts)
        plot (new-plot opt)
        value-axis (train-linear-scale plot (conj values 0) :y)
        label-axis (train-discrete-scale plot labels :x)
        init-fns [draw-plot-bg
                  [plot label-axis value-axis
                   :draw-grid-x false
                   :draw-grid-y false]]
        draw-fns [draw-data-bars [plot label-axis value-axis labels values opt]
                  draw-grid [plot value-axis :grid-color (getopt :plot-area-bg-color)] ]]
    (defapplet a-bar-plot
      :title (getopt :title)
      :setup (setup (assoc opt :init-fns init-fns))
      :draw (draw-data (concat init-fns draw-fns ))
      :size [ (:x  (getopt :graph-size)) (:y  (getopt :graph-size)) ])
    ;; run applet
    (run a-bar-plot  :interactive)
    a-bar-plot
    ))

