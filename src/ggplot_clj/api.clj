(ns ggplot-clj.api
  (:use [clojure repl pprint [string :only (split)]]
        [ggplot-clj constants core] 
        rosado.processing.applet
        rosado.processing)
  (:import [ggplot_clj.core Plot Vector Scale]))

;; data:  a map {variable name, values} all values should be
;;              sequences of equal length
;; aes:  binds a variable to a visual propery (aestetihc) through a scale
;; geom:  geometrical object (point, line, bar...)
;; stat: transformation to apply (binning, average...)
(defrecord Layer [data aes geom stat position options])

(defn get-layer
  "Convenience function to create a new layer."
  [ & { :keys [data aes geom stat position]
       :or {data {} aes {} stat :identity position :fix}
       :as args}]
  (if (nil? geom)
    (pprint "arg geom must be given")
    (Layer. data aes geom stat position (dissoc args :data :aes :stat :position ))))

(defn setup
  "Return setup function for the processing plot.
   Valid options:
    :init-fns -a list of functions (with no arguments), which are
         evaluated at the setup
   :plot-bg-color -rgb values, a seq of length 3
   :framerate -plot update frequency. nil means static plot"
  [ & opts]
  (let [opt (coll2map opts)]  
    (fn []
      (apply background-float (getopt :plot-bg-color))
      (smooth)
      (if (getopt :framerate)
        (framerate  (getopt :framerate))
        (no-loop))
      (doseq [ f (getopt :init-fns)] (f)) )))

(defn new-plot
  "create a new plot"
  [& opts]
  (let [opt (coll2map opts)]
    (Plot.
     (getopt :graph-up-left)
     (getopt :graph-size)
     (getopt :plot-margins))))

(defn  geom-point
  "Convenience function to create a layer for point geoms"
  [ & { :keys [data aes stat position]
       :or {data {} aes {} stat :identity position :fix}
       :as args}]
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
  (let [var-names (map #(get aes %) vars)]
    (map #(get data % nil) var-names )))

(defmulti get-draw-fn
  "Get the drawing function for a given geom"
  (fn [geom aes position data scales & opts ] geom))

(defmethod get-draw-fn :point
  ;; Function to plot data points
  [_ aes position data scales & opts]
  (let [  dt (get-data data aes :x :y :color :size)
        scl (map #(get scales % nil) [:x :y :color :size])]
    ;;(pprint (count  (concat scl dt opts)))
    (fn []  (apply  draw-data-points (concat scl dt (-> opts seq flatten))))))

(defmethod get-draw-fn :line
  ;;Function to plot data line
  [_ aes position data scales & opts]
  (let [  dt (get-data data aes :x :y )
        scl (map #(get scales % nil) [:x :y ])]
    ;;(pprint (count  (concat scl dt opts)))
    (fn []  (apply  draw-data-line (concat scl dt (-> opts seq flatten))))))

(defmethod get-draw-fn :bar
  ;;Function for bar plot
  [_ aes position data scales & opts]
  (let [  dt (get-data data aes :label :height )
        scl (map #(get scales % nil) [:label :height ])]
    (fn []  (apply  draw-data-bars (concat scl dt (-> opts seq flatten))))))

(defn compose-draw-fns [ & { :keys [ layers scales data] :as opts } ]
  (let [opt  (dissoc opts :layers :scales :data)
        all-data (reduce merge data (map :data layers))]
    ;;(pprint all-data)
    (for [layer layers]
      (get-draw-fn
       (:geom layer) (:aes layer) (:position layer)
       all-data scales (merge  opt (:options layer))))))

 
(defn- one-dimension-data
  [layers data dimension]
  (reduce concat  (for [layer layers]
                    (->> (get-data data (:aes layer) dimension) flatten (remove nil?) )))) 

(defn train-scales
  "train all scales"
  [^Plot plot layers data & opts]
  (let [opt (coll2map opts)
        all-data (reduce merge data (map :data layers))
        all-x (one-dimension-data layers all-data :x)
        all-y (one-dimension-data layers all-data :y)
        all-label (one-dimension-data layers all-data :label)
        all-height (conj  (one-dimension-data layers all-data :height) 0)
        all-size (one-dimension-data layers all-data :size) 
        x-axis (if (empty? all-x) nil  (train-linear-scale plot all-x :x))
        y-axis (if (empty? all-y) nil (train-linear-scale plot all-y :y))
        label-axis (if (empty? all-label) nil  (train-discrete-scale plot all-label :x))
        height-axis (if (empty? all-height) nil  (train-linear-scale plot all-height :y))
        size-scale (if (empty? all-size) nil (train-size-scale all-size opt))]
    {:x x-axis :y y-axis :label label-axis :height height-axis :size size-scale}))

(defn scatter-plot [ x-seq y-seq & opts]
  (let [opt  (coll2map opts)
        plot (new-plot opt)
        x-axis (train-linear-scale plot x-seq :x)
        y-axis (train-linear-scale plot y-seq :y)
        layer (geom-point
               :data {:var1 x-seq :var2 y-seq}
               :aes {:x :var1 :y :var2})
        draw-fns (compose-draw-fns
                  :layers [layer]
                  :scales {:x x-axis :y y-axis} )
        init-fns [(fn [] (draw-plot-bg plot x-axis y-axis))]]

    {:setup (setup (assoc opt :init-fns init-fns)),
     :draw (fn [] (doseq  [f (concat  init-fns draw-fns )] (f) ))}  ))

(defmacro run-ggplot
  [app-name graph & opts]
  `(let [~'opt (coll2map ~@opts)]
    (defapplet ~app-name
       :title (getopt :title)
       :setup (:setup ~graph)
       :draw (:draw ~graph)
       :size [ (:x  (getopt :graph-size))
               (:y  (getopt :graph-size))])
    (run ~app-name :interactive)))

(comment 
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

  (defn draw-data
  "A wrapper to run all listed functions on each update
  of the ggplot applet.
  draw-fns is a seq of pairs (function, its arguments) which are the
  executed at each update"
  [draw-fns]
  (fn []
    (doseq [[f args] (partition 2  draw-fns)]  (apply f args) )))
  )

(defn make-plot
  "Combine necessary drawing functions
  for a graph, given data, layers and options.
  Returns a map wher drawing functions are stored under keys
  :setup and :draw. The map is ment for the input to run-ggplot
  macro"
  [data layers & opts ]
  (let [opt (coll2map opts)
        plot (new-plot opt)
        scales (train-scales plot layers data)
        draw-fns (compose-draw-fns
                  :layers layers
                  :data data
                  :scales scales)
        xaxis (or (:x scales) (:label scales))
        yaxis (or (:y scales) (:height scales))
        init-fns [(fn [] (draw-plot-bg plot xaxis yaxis))] ]
    ;;    scales
    {:setup (setup (assoc opt :init-fns init-fns)),
     :draw (fn [] (doseq  [f (concat  init-fns draw-fns )] (f) ))}
    ))
