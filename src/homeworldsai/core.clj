(ns homeworldsai.core
  (:gen-class))

(defonce pyramids (for [colour ["r" "g" "b" "y"] size ["1" "2" "3"]] (keyword (.concat colour size))))

;; Three of everything
(defonce initial-bank (zipmap pyramids (repeat 3)))

;; Return a representation of the starting position.
(defonce initial-position
  {:turn :player1, :worlds {}
   :bank initial-bank})

;; --------- Testing support code
(defn get-pyramids-in-world [world]
  (reduce concat (map world [:stars :player1 :player2])))

(defn rebuild-bank [position]
  (let [used-pyramids (mapcat get-pyramids-in-world (vals (:worlds position)))]
    (reduce (fn [m k] (update m k dec)) initial-bank used-pyramids)))

(defn create-sample-position
  "Something to work with."
  []
  (let [pos (assoc initial-position :worlds
               {0 {:stars [:b1 :y2] :player1 [:g3 :g1] :name "alice"},
                1 {:stars [:b2 :g3] :player2 [:y3 :b2] :name "bob"},
                2 {:stars [:b3] :player1 [:y1 :g1] :name "moon"},
                3 {:stars [:g1] :player2 [:y1] :name "farside"},
                4 {:stars [:r1] :player1 [:y2] :player2 [:b3] :name "combat"}})]
    (assoc pos :bank (rebuild-bank pos))))

;; -----------------------

(defn find-smallest-piece [position ship]
  "Find the smallest piece available in the bank or nil if none."
  (let [bank (:bank position)
        colour (second (str ship)) 
        pieces (for [size [1 2 3]] (keyword (str colour size)))]
    (first (filter (fn [piece] (pos? (piece bank))) pieces))))

(defn perform-build
  "Build a new ship in a given world."
  [position ship world-key]
  (let [smallest-piece (find-smallest-piece position ship)
        player (:turn position)]
    (update-in (update-in position [:worlds world-key player] conj smallest-piece) [:bank smallest-piece] dec)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
