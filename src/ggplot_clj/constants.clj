(ns ggplot-clj.constants
  (:use [clojure repl pprint]))

(def x-size 750)
(def y-size 550)

(def solarized
  {:base03    [0x002b36   0  43  54 ]
   :base02    [0x073642    7  54  66] 
   :base01    [0x586e75   88 110 117] 
   :base00    [0x657b83 101 123 131] 
   :base0     [0x839496  131 148 150] 
   :base1     [0x93a1a1  147 161 161] 
   :base2     [0xeee8d5 238 232 213]  
   :base3     [0xfdf6e3  253 246 227]  
   :yellow    [0xb58900   181 137   0]  
   :orange    [0xcb4b16  203  75  22]  
   :red       [0xdc322f   220  50  47]   
   :magenta  [ 0xd33682   211  54 130] 
   :violet    [0x6c71c4  108 113 196] 
   :blue      [0x268bd2   38 139 210] 
   :cyan      [0x2aa198   42 161 152] 
   :green     [0x859900  133 153   0]})  

(defn solarized-rgb [color]
  (map (color solarized)  [1 2 3]))


