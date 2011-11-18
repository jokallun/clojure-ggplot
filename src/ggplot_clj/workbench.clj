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

(defn make-plot [data layers & opts ]
  (let [opt (coll2map opts)
        plot (new-plot opt)
        scales (train-scales plot layers data)
        draw-fns (compose-draw-fns
                  :layers layers
                  :data data
                  :scales scales )
        init-fns [(fn [] (draw-plot-bg plot (:x scales) (:y scales)))] ]
    {:setup (setup (assoc opt :init-fns init-fns)),
     :draw (fn [] (doseq  [f (concat  init-fns draw-fns )] (f) ))}))

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


(stop aname)

