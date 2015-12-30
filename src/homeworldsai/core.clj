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

(defn rebuild-bank-in-position [position]
  (assoc position :bank (rebuild-bank position)))

(defn create-sample-position
  "Something to work with."
  []
  (let [pos (assoc initial-position :worlds
                   {0 {:stars [:b1 :y2] :player1 [:g3 :g1] :name "alice"},
                    1 {:stars [:b2 :g3] :player2 [:y3 :b2] :name "bob"},
                    2 {:stars [:b3] :player1 [:y1 :g1] :name "moon"},
                    3 {:stars [:g1] :player2 [:y1] :name "farside"},
                    4 {:stars [:r1] :player1 [:y2] :player2 [:b3] :name "combat"}}
                    :next-world 5)]
    (assoc pos :bank (rebuild-bank pos))))

;; -----------------------

(defn other-player [player]
  (if (= :player1 player) :player2 :player1))

(defn find-smallest-piece
  "Find the smallest piece available in the bank or nil if none."
  [position ship]
  (let [bank (:bank position)
        colour (second (str ship)) 
        pieces (for [size [1 2 3]] (keyword (str colour size)))]
    (first (filter (fn [piece] (pos? (piece bank))) pieces))))

(defn perform-build
  "Build a new ship in a given world."
  [position ship world-key]
  (let [smallest-piece (find-smallest-piece position ship)
        player (:turn position)]
    (-> position
        (update-in [:worlds world-key player] conj smallest-piece)
        (update-in [:bank smallest-piece] dec))))

(defn remove-one-ship [col ship]
  (let [[before after] (split-with #(not= % ship) col)]
    (concat before (rest after))))

(defn replace-ship [col old-ship new-ship]
  (conj (remove-one-ship col old-ship) new-ship))

(defn perform-trade
  "Trade an existing ship with another colour if available."
  [position old-ship new-ship world-key]
  (update-in position [:worlds world-key (:turn position)] replace-ship old-ship new-ship))

(defn perform-attack
  "A player's ship is captured by the attacker."
  [position attacking-ship victim-ship world-key]
  (let [removed (update-in position [:worlds world-key (other-player (:turn position))] remove-one-ship victim-ship)]
    (update-in removed [:worlds world-key (:turn position)] conj victim-ship)))

(defn perform-move
  "A player's ship moves from one world to another."
  [position ship source-world-key dest-world-key]
  (let [removed (update-in position [:worlds source-world-key (:turn position)] remove-one-ship ship)]
    (update-in removed [:worlds dest-world-key (:turn position)] conj ship)))

(defn create-world [star]
  {:stars [star] :player1 [] :player2 []})

(defn perform-discover
  "A player's ship moves to a newly discovered world."
  [position ship source-world-key star]
  (let [removed (update-in position [:worlds source-world-key (:turn position)] remove-one-ship ship)
        new-world (create-world star)
        dest-world-key (:next-world position)]
    (-> removed
        (assoc-in [:worlds dest-world-key] new-world)
        (update-in [:worlds dest-world-key (:turn position)] conj ship)
        (update-in [:next-world] inc)
        rebuild-bank-in-position)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
