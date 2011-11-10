(ns ggplot-clj.workbench 
  (:use [clojure repl pprint]
        [rosado.processing]
        [rosado.processing.applet]
        [ggplot-clj constants api] :reload-all)
  (:import [ggplot_clj.core Plot Vector Scale]))

(def sample-plot
  (Plot.
   (Vector. 0 y-size)
   (Vector. x-size y-size)
   {:top 0.05 :bottom 0.075 :left 0.075 :right 0.05}   ))


(def x-seq (range 0 10 0.1))

(def y-seq
  (let [tmp (repeatedly (count x-seq)  #(* 0.05 (-  (rand-int 10) 5)))]
    (loop [x 0 y tmp s []]
      (if (empty? y) s
          (let [this (+ x (first y))] 
            (recur  this (rest y) (conj s this)))))))

(scatter-plot x-seq y-seq
              :plot-type :points
              :graph-size (Vector. 700 500)
              :point-color [220 50 47]
              :framerate 1)

;;(solarized-rgb :red)
  
(stop ggplot)

