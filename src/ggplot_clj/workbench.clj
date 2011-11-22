(ns ggplot-clj.workbench 
  (:use [clojure repl pprint]
        [rosado.processing.applet]
        [ggplot-clj constants api core] :reload-all)
  (:import [ggplot_clj.core Plot Vector Scale]
           [ggplot_clj.api Layer]))
 
(def x-seq (range 0 1 0.01))

(def rnds (repeatedly (fn [] (- 0.0025 (rand 0.005)))))

(def y-seq-1
  (let [adder (fn [lst el]  (conj  lst  (+ (first lst) el))  )]
              (reduce adder '(0) (take (dec  (count x-seq)) rnds) )))

(def y-seq-2
  (let [len (dec  (count x-seq))
        adder (fn [lst el]  (conj  lst  (+ (first lst) el))  )]
    (->  (reduce adder '(0) (take len (drop len  rnds)) )
         reverse vec)))

(def size-seq
  (let [len (dec  (count x-seq))
        adder (fn [lst el]  (conj  lst  (+ (first lst) el))  )]
    (->  (reduce adder '(0) (take len (drop (* 2 len)  rnds)) )
         reverse vec)))



(let [ opt  {:graph-size (Vector. 450 350) }
      plot-data {:var1 x-seq :var2 y-seq-1 :var3 y-seq-2 :var4 size-seq}
      pnt (get-layer
           :aes {:x :var1 :y :var3 :size :var4}
           :geom :point)
      lne (get-layer
           :aes {:x :var1 :y :var2}
           :geom :line
           :line-color (solarized-rgb :red))
      applet-map (make-plot plot-data [pnt lne] opt)]
  (run-ggplot aname  applet-map opt ))

(let [ opt  {:graph-size (Vector. 750 450) :framerate 1 }
      plot-data {:dum1 ["a" "bb" "ccc"] :dum2 [10 20 30]}
      pnt (get-layer
           :aes {:label :dum1 :height :dum2 }
           :geom :bar)
      applet-map (make-plot plot-data [pnt] opt)]
  (run-ggplot aname  applet-map opt )
  ;;applet-map
  )


(stop aname)

