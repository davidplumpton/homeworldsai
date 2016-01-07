(ns homeworldsai.core
  (:gen-class))

(defonce all-colours ["r" "g" "b" "y"])

(defonce pyramids (for [colour all-colours size ["1" "2" "3"]] (keyword (.concat colour size))))

(defonce players [:player1 :player2])

;; Three of everything
(defonce initial-bank (zipmap pyramids (repeat 3)))

;; Return a representation of the starting position.
(defonce initial-position
  {:turn :player1, :worlds {}
   :next-world 0, :bank initial-bank})

(defn get-pyramids-in-world [world]
  (mapcat world (conj players :stars)))

(defn rebuild-bank [position]
  (let [used-pyramids (mapcat get-pyramids-in-world (vals (:worlds position)))]
    (reduce (fn [m k] (update m k dec)) initial-bank used-pyramids)))

(defn rebuild-bank-in-position [position]
  (assoc position :bank (rebuild-bank position)))

;; --------- Testing support code
(defn create-sample-position
  "Something to work with."
  []
  (let [pos (assoc initial-position :worlds
                   {0 {:stars [:b1 :y2] :player1 [:g3 :g1] :name "alice" :key 0},
                    1 {:stars [:b2 :g3] :player2 [:y3 :b2] :name "bob" :key 1},
                    2 {:stars [:b3] :player1 [:y1 :g1] :name "moon" :key 2},
                    3 {:stars [:g1] :player2 [:y1] :name "farside" :key 3},
                    4 {:stars [:r1] :player1 [:y2] :player2 [:b3] :name "combat" :key 4}}
                  :next-world 5)]
    (rebuild-bank-in-position pos)))

;; -----------------------

;; Needs to be extended to multiple players
(defn other-player [player]
  (if (= :player1 player) :player2 :player1))

(defn get-colour
  "Return the colour as a string."
  [pyramid]
  (.substring (str pyramid) 1 2))

(defn get-size
  "Return the size of a pyramid as a string."
  [pyramid]
  (.substring (str pyramid) 2 3))

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

(defn remove-ships-of-colour
  "Remove all ships of this colour from the collection."
  [col colour]
  (filter #(not= colour (get-colour %)) col))

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

(defn get-pyramid-matching-colour
  "Return a pyramid matching the colour, or nil."
  [pyramids colour]
  (some #(when (= colour (get-colour %)) %) pyramids))

(defn return-star-to-bank
  "Return a star to the bank, removing the world if it is the last star.
  Pass a colour if a star is catastrophe needs to choose from possibly
  two homeworld stars."
  [position world-key & {:keys [colour]}]
  (let [stars (get-in position [:worlds world-key :stars])]
    (if (= 2 (count stars))
      (let [star (get-pyramid-matching-colour stars colour)]
        (-> position
            (update-in [:worlds world-key :stars] (fn [stars] (remove #(= colour (get-colour %)) stars)))
            (update-in [:bank star] inc)))
      (let [star (first stars)]
        (-> position
            (update-in [:worlds] dissoc world-key)
            (update-in [:bank star] inc))))))

(defn return-star-to-bank-if-empty [position world-key]
  (if (nil? (get-in position [:worlds world-key]))
    position
    (if (every? empty? (concat (vals (select-keys (get-in position [:worlds world-key]) players))))
      (return-star-to-bank position world-key)
      position)))

(defn perform-move
  "A player's ship moves from one world to another. Return a star to the bank if necessary."
  [position ship source-world-key dest-world-key]
  (let [removed (update-in position [:worlds source-world-key (:turn position)] remove-one-ship ship)
        ship-moved (update-in removed [:worlds dest-world-key (:turn position)] conj ship)]
    (return-star-to-bank-if-empty ship-moved source-world-key)))

(defn create-world [stars key]
  "Define the structure of a world."
  (into
    {:stars stars :key key}
    (for [player players] [player []])))

(defn perform-discover
  "A player's ship moves to a newly discovered world. Return a star to the bank if necessary."
  [position ship source-world-key star]
  (let [removed (update-in position [:worlds source-world-key (:turn position)] remove-one-ship ship)
        dest-world-key (:next-world position)
        new-world (create-world [star] dest-world-key)]
    (-> removed
        (assoc-in [:worlds dest-world-key] new-world)
        (update-in [:bank star] dec)
        (update-in [:worlds dest-world-key (:turn position)] conj ship)
        (update-in [:next-world] inc)
        (return-star-to-bank-if-empty source-world-key))))

(defn perform-sacrifice
  "Return a ship to the bank."
  [position ship world-key]
  (-> position
      (update-in [:worlds world-key (:turn position)] remove-one-ship ship)
      (update-in [:bank ship] inc)
      (return-star-to-bank-if-empty world-key)))

(defn remove-ships-and-maybe-a-star
  [position colour world-key]
  (if (get-pyramid-matching-colour (get-in position [:worlds world-key :stars]) colour)
    (return-star-to-bank position world-key :colour colour)
    (reduce
      (fn removing-ships [position player] (update-in position [:worlds world-key player] remove-ships-of-colour colour))
      position players)))

(defn perform-catastrophe
  "Four or more things of one colour in a system, so they get removed."
  [position colour world-key]
  (-> position
    (remove-ships-and-maybe-a-star colour world-key)
    (return-star-to-bank-if-empty world-key)
    (rebuild-bank-in-position)))

(defn colour-available-in-world?
  "Is there a star or one of the players ships of this colour in the world?"
  [world colour player]
  (or
    (get-pyramid-matching-colour (:stars world) colour)
    (get-pyramid-matching-colour (player world) colour)))

(defn create-move
  "Represent a possible move with its parameters."
  [& {:keys [:move-type :source-world :colour :ship :dest-world :target-ship]}]
  (conj
    {:move-type move-type :source-world source-world :ship ship
     :colour colour :dest-world dest-world :target-ship target-ship}))

(defn find-all-create-moves
  "For every world with green available make a create move for each player ship colour."
  [position player]
  (for [world (vals (:worlds position))
        :when (colour-available-in-world? world "g" player) 
        colour (distinct (map get-colour (player world)))]
    (create-move :move-type :create :source-world (:key world) :colour colour)))

(defn find-all-trade-moves
  "For every world with blue available make a trade move for each player ship colour and size."
  [position player]
  (let [bank (:bank position)]
    (for [world (vals (:worlds position))
          :when (colour-available-in-world? world "b" player)
          ship (distinct (player world))
          target-colour all-colours
          :when (not= target-colour (get-colour ship))
          :let [target-ship (keyword (str target-colour (get-size ship)))]
          :when (pos? (target-ship bank))]
      (create-move :move-type :trade :source-world (:key world) :ship ship :target-ship target-ship))))

(defn find-all-attack-moves
  "For every world with red available make an attack move for each enemy ship not larger than the largest ship."
  [position player]
  (for [world (vals (:worlds position))
        :when (colour-available-in-world? world "r" player) 
        :when (pos? (count (player world)))
        :let [biggest-ship-size (reduce max (map get-size (player world)))]
        enemy-ship (get world (other-player player))
        :when (>= 0 (.compareTo biggest-ship-size (get-size enemy-ship)))]
    (create-move :move-type :attack :source-world (:key world) :target-ship enemy-ship)))

(defn can-navigate-between-worlds?
  "Ships can navigate between worlds if there are no stars of the same size."
  [world1 world2]
  (let [world1-sizes (map get-size (:stars world1))
        world2-sizes (map get-size (:stars world2))
        all-sizes (concat world1-sizes world2-sizes)]
    (= (count all-sizes) (count (distinct all-sizes)))))

(defn can-discover?
  "Can discover a new world if the size is different to any of the stars in the source world."
  [world1 size]
  (not (some #(= size (get-size %)) (:stars world1))))

(defn find-all-move-moves
  "For every world with yellow available for each distinct ship for each world that the ship can navigate to."
  [position player]
  (for [world (vals (:worlds position))
        :when (colour-available-in-world? world "y" player)
        ship (distinct (player world))
        target-world (vals (:worlds position))
        :when (can-navigate-between-worlds? target-world world)]
    (create-move :move-type :move :ship ship :source-world (:key world) :dest-world (:key target-world))))

(defn find-all-discover-moves
  "For every world with yellow available for each distinct ship for each distinct pyramid available in the bank that it can navigate to."
  [position player]
  (let [bank (:bank position)]
    (for [world (vals (:worlds position))
          :when (colour-available-in-world? world "y" player)
          ship (distinct (player world))
          target-world-star (keys (:bank position))
          :when (can-discover? world (get-size target-world-star))
          :when (pos? (target-world-star bank))]
      (create-move :move-type :discover :ship ship :source-world (:key world) :dest-world target-world-star))))

(defn find-all-possible-moves
  [position]
  (let [player (:turn position)]
    (concat
      (find-all-create-moves position player)
      (find-all-trade-moves position player)
      (find-all-attack-moves position player)
      (find-all-move-moves position player)
      (find-all-discover-moves position player))))

(defn perform-homeworld
  "Initial move to establish a homeship with two stars and ship."
  [position star1 star2 ship]
  (let [new-world-key (:next-world position)]
    (-> position
        (assoc-in [:worlds new-world-key] (create-world [star1 star2] new-world-key))
        (update-in [:worlds new-world-key (:turn position)] conj ship)
        (update-in [:next-world] inc)
        rebuild-bank-in-position)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
