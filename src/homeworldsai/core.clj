(ns homeworldsai.core
  (:gen-class))

(defonce all-colours ["r" "g" "b" "y"])

;; Use keywords to represent pyramids, e.g. :b2 is a medium blue.

(defonce pyramids (for [colour all-colours size ["1" "2" "3"]] (keyword (str colour size))))

(defonce players [:player1 :player2])

;; Three of each pyramid.

(defonce initial-bank (zipmap pyramids (repeat 3)))

;; Return a representation of the starting position.

(defonce initial-position
  {:turn :player1, :worlds {}
   :next-world 0, :bank initial-bank})

(defn get-pyramids-in-world
  "Get all the pyramids in a world, i.e. all players and the stars."
  [world]
  (mapcat world (conj players :stars)))

(defn rebuild-bank
  "Convenience function to reconstruct the bank based on the current position."
  [position]
  (let [used-pyramids (mapcat get-pyramids-in-world (vals (:worlds position)))]
    (reduce (fn [m k] (update m k dec)) initial-bank used-pyramids)))

(defn rebuild-bank-in-position
  "Update the position with the rebuilt bank."
  [position]
  (assoc position :bank (rebuild-bank position)))

;; Needs to be extended to multiple players

(defn other-player [player]
  (if (= :player1 player) :player2 :player1))

(defn next-turn
  "It's now the other player's turn."
  [position]
  (update-in position [:turn] other-player))

;; ---------
;; ## Testing support code

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

(declare perform-homeworld)

(defn create-start-position
  "A sample start position (i.e. just the homeworlds)."
  []
  (-> initial-position
      (perform-homeworld :r1 :b2 :g3)
      next-turn
      (perform-homeworld :y1 :b3 :g3)
      next-turn
      rebuild-bank-in-position))

;; -----------------------

(defn get-colour
  "Return the colour as a string."
  [pyramid]
  (.substring (str pyramid) 1 2))

(defn get-size
  "Return the size of a pyramid as a string."
  [pyramid]
  (.substring (str pyramid) 2 3))

(defn get-size-int
  "Return the pyramid size as an int."
  [pyramid]
  (Integer/parseInt (get-size pyramid)))

(defn find-smallest-piece
  "Find the smallest piece available in the bank or nil if none."
  [position ship]
  (let [bank (:bank position)
        colour (second (str ship))
        pieces (for [size [1 2 3]] (keyword (str colour size)))]
    (first (filter (fn [piece] (pos? (piece bank))) pieces))))

;; # Code for carrying out the various kinds of moves

(defn perform-build
  "Build a new ship in a given world."
  [position ship world-key]
  (let [smallest-piece (find-smallest-piece position ship)
        player (:turn position)]
    (-> position
        (update-in [:worlds world-key player] conj smallest-piece)
        (update-in [:bank smallest-piece] dec))))

(defn remove-one-pyramid
  "There doesn't seem to be a standard way of just removing a single item from a collection."
  [col pyramid]
  (let [[before after] (split-with #(not= % pyramid) col)]
    (concat before (rest after))))

(defn remove-pyramids-of-colour
  "Remove all pyramids of this colour from the collection."
  [col colour]
  (filter #(not= colour (get-colour %)) col))

(defn replace-pyramid
  "In the collection replace the old pyramid with a new one."
  [col old-pyramid new-pyramid]
  (conj (remove-one-pyramid col old-pyramid) new-pyramid))

(defn perform-trade
  "Trade an existing ship with another colour if available."
  [position old-ship new-ship world-key]
  (-> position
      (update-in [:worlds world-key (:turn position)] replace-pyramid old-ship new-ship)
      (update-in [:bank old-ship] inc)
      (update-in [:bank new-ship] dec)))

(defn perform-attack
  "A player's ship is captured by the attacker."
  [position attacking-ship victim-ship world-key]
  (-> position
      (update-in [:worlds world-key (other-player (:turn position))] remove-one-pyramid victim-ship)
      (update-in [:worlds world-key (:turn position)] conj victim-ship)))

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

(defn return-star-to-bank-if-empty
  "If there are no ships in this world then the star should be returned to the bank."
  [position world-key]
  (if (nil? (get-in position [:worlds world-key]))
    position
    (if (every? empty? (concat (vals (select-keys (get-in position [:worlds world-key]) players))))
      (return-star-to-bank position world-key)
      position)))

;; Move and discover are treated as separate kinds of moves due to the different
;; rules that must be considered.

(defn perform-move
  "A player's ship moves from one world to another. Return a star to the bank if necessary."
  [position ship source-world-key dest-world-key]
  (-> position
      (update-in [:worlds source-world-key (:turn position)] remove-one-pyramid ship)
      (update-in [:worlds dest-world-key (:turn position)] conj ship)
      (return-star-to-bank-if-empty source-world-key)))

(defn create-world
  "Define the structure of a world."
  [stars key]
  (into
    {:stars stars :key key}
    (for [player players] [player []])))

(defn perform-discover
  "A player's ship moves to a newly discovered world. Return a star to the bank if necessary."
  [position ship source-world-key star]
  (let [dest-world-key (:next-world position)
        new-world (create-world [star] dest-world-key)]
    (-> position
        (update-in [:worlds source-world-key (:turn position)] remove-one-pyramid ship)
        (assoc-in [:worlds dest-world-key] new-world)
        (update-in [:bank star] dec)
        (update-in [:worlds dest-world-key (:turn position)] conj ship)
        (update-in [:next-world] inc)
        (return-star-to-bank-if-empty source-world-key))))

(defn perform-sacrifice
  "Return a ship to the bank."
  [position ship world-key]
  (-> position
      (update-in [:worlds world-key (:turn position)] remove-one-pyramid ship)
      (update-in [:bank ship] inc)
      (return-star-to-bank-if-empty world-key)))

(defn remove-ships-and-maybe-a-star
  "Remove the ships and/or star matching the colour from this world."
  [position colour world-key]
  (if (get-pyramid-matching-colour (get-in position [:worlds world-key :stars]) colour)
    (return-star-to-bank position world-key :colour colour)
    (reduce
      (fn removing-ships [position player] (update-in position [:worlds world-key player] remove-pyramids-of-colour colour))
      position players)))

(defn perform-catastrophe
  "Four or more things of one colour in a system, so they get removed."
  [position colour world-key]
  (-> position
      (remove-ships-and-maybe-a-star colour world-key)
      (return-star-to-bank-if-empty world-key)
      (rebuild-bank-in-position)))

(defmulti play-move
  "Allows for the various kinds of moves, based on `:move-type`"
  (fn [_ move] (:move-type move)))

(defmethod play-move :build
  [position move]
  (perform-build position (:ship move) (:source-world move)))

(defmethod play-move :trade
  [position move]
  (perform-trade position (:ship move) (:target-ship move) (:source-world move)))

(defmethod play-move :attack
  [position move]
  (perform-attack position (:ship move) (:target-ship move) (:source-world move)))

(defmethod play-move :move
  [position move]
  (perform-move position (:ship move) (:source-world move) (:dest-world move)))

(defmethod play-move :discover
  [position move]
  (perform-discover position (:ship move) (:source-world move) (:dest-world move)))

(defmethod play-move :sacrifice
  [position move]
  (perform-sacrifice position (:ship move) (:source-world move)))

(defn play-single-move
  "Play a move and change the player to move next time."
  [position move]
  (-> position
      (play-move move)
      (update-in [:turn] other-player)))

;; ## Code for finding the possible moves

(defn colour-available-in-world?
  "Is there a star or one of the players ships of this colour in the world?"
  [world colour player]
  (or
    (get-pyramid-matching-colour (:stars world) colour)
    (get-pyramid-matching-colour (player world) colour)))

(defn create-move
  "Represent a possible move with its parameters."
  [& {:keys [:move-type :source-world :colour :ship :dest-world :target-ship]}]
  {:move-type move-type :source-world source-world :ship ship
     :colour colour :dest-world dest-world :target-ship target-ship})

;; Much of the following code uses `for` clauses to find all the combinations
;; subject to certain restrictions expressed by `:when`

(defn find-all-build-moves
  "For every world with green available make a create move for each player ship colour."
  [position]
  (let [player (:turn position)]
    (for [world (vals (:worlds position))
          :when (colour-available-in-world? world "g" player)
          :let [ships (player world)]
          colour (distinct (map get-colour ships))
          :let [ship (some #(when (= colour (get-colour %)) %) ships)]]
      (create-move :move-type :build :source-world (:key world) :colour colour :ship ship))))

(defn find-all-trade-moves
  "For every world with blue available make a trade move for each player ship colour and size."
  [position]
  (let [player (:turn position)
        bank (:bank position)]
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
  [position]
  (let [player (:turn position)]
    (for [world (vals (:worlds position))
          :when (pos? (count (player world)))
          :when (colour-available-in-world? world "r" player)
          :let [biggest-ship-size (reduce max 0 (map get-size-int (player world)))]
          enemy-ship (distinct (get world (other-player player)))
          :when (>= biggest-ship-size (get-size-int enemy-ship))]
      (create-move :move-type :attack :source-world (:key world) :target-ship enemy-ship))))

(defn abandon-homeworld?
  "True if the last ship is being moved out (or sacrificed) of the homeworld."
  [position source-world-key]
  (let [player (:turn position)
        ships (get-in position [:worlds source-world-key player])]
    (and
      (= 1 (count ships))
      (or
        (and (= player :player1) (= source-world-key 0))
        (and (= player :player2) (= source-world-key 1))))))

(defn can-navigate-between-worlds?
  "Ships can navigate between worlds if there are no stars of the same size.
  Must not abandon the homeworld (world1 is the source world)."
  [position world1 world2]
  (let [world1-sizes (map get-size (:stars world1))
        world2-sizes (map get-size (:stars world2))
        all-sizes (concat world1-sizes world2-sizes)]
    (and
      (= (count all-sizes) (count (distinct all-sizes)))
      (not (abandon-homeworld? position (:key world1))))))

(defn can-discover?
  "Can discover a new world if the size is different to any of the stars in the source world.
  The target world star must be in the bank and the homeworld must not be abandoned."
  [position world target-world-star]
  (let [size (get-size (world (:turn position)))
        target-world-size (get-size target-world-star)]
    (and
      (pos? (target-world-star (:bank position)))
      (not (abandon-homeworld? position (:key world)))
      (not (some #(= size (get-size %)) (:stars world))))))

(defn find-all-move-moves
  "For every world with yellow available for each distinct ship for each world that the ship can navigate to."
  [position]
  (let [player (:turn position)]
    (for [world (vals (:worlds position))
          :when (colour-available-in-world? world "y" player)
          ship (distinct (player world))
          target-world (vals (:worlds position))
          :when (can-navigate-between-worlds? position world target-world)]
      (create-move :move-type :move :ship ship :source-world (:key world) :dest-world (:key target-world)))))

(defn find-all-discover-moves
  "For every world with yellow available for each distinct ship for each distinct pyramid available in the bank that it can navigate to."
  [position]
  (let [player (:turn position)
        bank (:bank position)]
    (for [world (vals (:worlds position))
          :when (colour-available-in-world? world "y" player)
          ship (distinct (player world))
          target-world-star (keys bank)
          :when (can-discover? position world target-world-star)]
      (create-move :move-type :discover :ship ship :source-world (:key world) :dest-world target-world-star))))

;; The following code is concerned with sacrifice moves and their corresponding follow up moves.
;; It seems like there's always going to be quite a lot of complexity to handle.

(defn find-all-build-moves-after-position
  "From a list of positions and moves find more build moves.
  Return them as a longer list of positions and list of moves."
  [list-of-positions-and-moves]
  (for [pos-and-moves list-of-positions-and-moves
        :let [position (first pos-and-moves)
              moves (second pos-and-moves)]
        world (vals (:worlds position))
        :let [player (:turn position)
              ships (player world)]
        colour (distinct (map get-colour ships))
        :let [ship (some #(when (= colour (get-colour %)) %) ships)]
        :when (find-smallest-piece position ship)
        :let [move (create-move :move-type :build :ship ship :source-world (:key world) :colour colour)]]
    [(play-move position move) (conj moves move)]))

(defn find-all-trade-moves-after-position
  "From a list of positions and moves find more trade moves.
  Return them as a longer list of positions and list of moves."
  [list-of-positions-and-moves]
  (for [pos-and-moves list-of-positions-and-moves
        :let [position (first pos-and-moves)
              moves (second pos-and-moves)
              player (:turn position)
              bank (:bank position)]
        world (vals (:worlds position))
        ship (distinct (player world))
        target-colour all-colours
        :when (not= target-colour (get-colour ship))
        :let [target-ship (keyword (str target-colour (get-size ship)))]
        :when (pos? (target-ship bank))
        :let [move (create-move :move-type :trade :source-world (:key world) :ship ship :target-ship target-ship)]]
    [(play-move position move) (conj moves move)]))

(defn find-all-attack-moves-after-position
  "From a list of positions and moves find more attack moves.
  Return them as a longer list of positions and list of moves."
  [list-of-positions-and-moves]
  (for [pos-and-moves list-of-positions-and-moves
        :let [position (first pos-and-moves)
              moves (second pos-and-moves)
              player (:turn position)]
        world (vals (:worlds position))
        :when (pos? (count (player world)))
        :let [biggest-ship-size (reduce max 0 (map get-size-int (player world)))]
        enemy-ship (distinct (get world (other-player player)))
        :when (>= biggest-ship-size (get-size-int enemy-ship))
        :let [move (create-move :move-type :attack :source-world (:key world) :target-ship enemy-ship)]]
    [(play-move position move) (conj moves move)]))

(defn find-all-move-moves-after-position
  "From a list of positions and moves find more move and
  discover moves. Return them as a longer list of positions and list of moves."
  [list-of-positions-and-moves]
  (concat
    (for [pos-and-moves list-of-positions-and-moves
          :let [position (first pos-and-moves)
                moves (second pos-and-moves)
                player (:turn position)
                bank (:bank position)]
          world (vals (:worlds position))
          ship (distinct (player world))
          target-world (vals (:worlds position))
          :when (can-navigate-between-worlds? position world target-world)
          :let [move (create-move :move-type :move :ship ship :source-world (:key world) :dest-world (:key target-world))]]
      [(play-move position move) (conj moves move)])
    (for [pos-and-moves list-of-positions-and-moves
          :let [position (first pos-and-moves)
                moves (second pos-and-moves)
                player (:turn position)
                bank (:bank position)]
          world (vals (:worlds position))
          ship (distinct (player world))
          target-world-star (keys (:bank position))
          :when (can-discover? position world target-world-star)
          :let [move (create-move :move-type :discover :ship ship :source-world (:key world) :dest-world target-world-star)]]
      [(play-move position move) (conj moves move)])))

(defn find-all-sacrifice-moves
  "For every world, for every unique player ship of a colour perform a sacrifice and then find
  all combinations of that type of colour move."
  [position colour find-moves-fn]
  (let [player (:turn position)]
    (for [world (vals (:worlds position))
          :when (not (abandon-homeworld? position (:key world)))
          ship (distinct (player world))
          :when (= colour (get-colour ship))
          :let [ship-size (get-size-int ship)
                sacrifice-move  (create-move :move-type :sacrifice :ship ship :source-world (:key world))
                position-after-move (play-move position sacrifice-move)
                move-combos (as-> [[position-after-move []]] $
                                  (iterate find-moves-fn $)
                                  (nth $ ship-size)
                                  (map second $))]
          child-move-combo move-combos]
      (assoc sacrifice-move :child-moves child-move-combo))))

(defn find-all-possible-moves
  "Return a list of all possible moves from this position."
  [position]
  (concat
    (find-all-build-moves position)
    (find-all-trade-moves position)
    (find-all-attack-moves position)
    (find-all-move-moves position)
    (find-all-discover-moves position)
    (find-all-sacrifice-moves position "g" find-all-build-moves-after-position)
    (find-all-sacrifice-moves position "b" find-all-trade-moves-after-position)
    (find-all-sacrifice-moves position "r" find-all-attack-moves-after-position)
    (find-all-sacrifice-moves position "y" find-all-move-moves-after-position)))

(defn perform-homeworld
  "Initial move to establish a homeship with two stars and ship."
  [position star1 star2 ship]
  (let [new-world-key (:next-world position)]
    (-> position
        (assoc-in [:worlds new-world-key] (create-world [star1 star2] new-world-key))
        (update-in [:worlds new-world-key (:turn position)] conj ship)
        (update-in [:next-world] inc)
        rebuild-bank-in-position)))

;; ## Code for finding random moves for MCTS

(defn random-build
  "Construct a build move at random from the worlds where building can be performed."
  [position]
  (let [player (:turn position)
        bank (:bank position)
        combos (for [world (vals (:worlds position))
                     :when (colour-available-in-world? world "g" player)
                     ship (distinct (player world))
                     :when (pos? (ship bank))]
                 [world ship])]
    (when (seq combos)
      (let [[world ship] (rand-nth combos)]
        (create-move :move-type :build :source-world (:key world) :colour (get-colour ship) :ship ship)))))

(defn random-trade
  "Construct a trade move at random from the worlds where trading can be performed."
  [position]
  (let [player (:turn position)
        bank (:bank position)
        combos (for [world (vals (:worlds position))
                     :when (colour-available-in-world? world "b" player)
                     ship (distinct (player world))
                     :let [ship-colour (get-colour ship)]
                     target-colour all-colours
                     :when (not= target-colour ship-colour)
                     :let [target-ship (keyword (str target-colour (get-size ship)))]
                     :when (pos? (target-ship bank))]
                  [world ship target-ship])]
     (when (seq combos)
       (let [[world ship target-ship] (rand-nth combos)]
         (create-move :move-type :trade :source-world (:key world) :ship ship :target-ship target-ship)))))

(defn random-attack
  "Construct an attack move at random from the worlds where attacking can be performed."
  [position]
  (let [player (:turn position)
        combos (for [world (vals (:worlds position))
                     :when (pos? (count (player world)))
                     :when (colour-available-in-world? world "r" player)
                     target-ship (distinct (world (other-player player)))
                     :let [biggest-ship-size (reduce max 0 (map get-size-int (player world)))]
                     :when (>= biggest-ship-size (get-size-int target-ship))]
                 [world target-ship])]
    (when (seq combos)
      (let [[world target-ship] (rand-nth combos)]
        (create-move :move-type :attack :source-world (:key world) :target-ship target-ship)))))

(defn random-move
  "A random movement move."
  [position]
  (let [player (:turn position)
        combos (for [world (vals (:worlds position))
                     :when (colour-available-in-world? world "y" player)
                     ship (distinct (player world))
                     target-world (vals (:worlds position))
                     :when (can-navigate-between-worlds? position world target-world)]
                 [world ship target-world])]
    (when (seq combos)
      (let [[world ship target-world] (rand-nth combos)]
        (create-move :move-type :move :ship ship :source-world (:key world) :dest-world (:key target-world))))))

(defn random-discover
  "A random discover move."
  [position]
  (let [player (:turn position)
        bank (:bank position)
        combos (for [world (vals (:worlds position))
                     :when (colour-available-in-world? world "y" player)
                     ship (distinct (player world))
                     target-world-star (keys bank)
                     :when (can-discover? position world target-world-star)]
                 [world ship target-world-star])]
    (when (seq combos)
      (let [[world ship target-world-star] (rand-nth combos)]
        (create-move :move-type :discover :ship ship :source-world (:key world) :dest-world target-world-star)))))

(defn random-play
  "A random random move."
  [position]
  (let [r (rand)]
    (condp > r
      0.2 (random-build position)
      0.4 (random-trade position)
      0.6 (random-attack position)
      0.8 (random-move position)
      9.9 (random-discover position))))

;; ## Code for quickly counting the number of different moves

(defn count-build-moves
  "Count how many different build moves are possible.
  Just like finding the moves, but needs to be much faster."
  [position]
  (let [player (:turn position)
        total (atom 0)]
    (doseq [world (vals (:worlds position))
            :when (colour-available-in-world? world "g" player)
            :let [ships (player world)]
            colour (distinct (map get-colour ships))]
      (swap! total inc))
    @total))

(defn count-trade-moves
  "For every world with blue available make a trade move for each player ship colour and size."
  [position]
  (let [player (:turn position)
        bank (:bank position)
        total (atom 0)]
    (doseq [world (vals (:worlds position))
            :when (colour-available-in-world? world "b" player)
            ship (distinct (player world))
            target-colour all-colours
            :when (not= target-colour (get-colour ship))
            :let [target-ship (keyword (str target-colour (get-size ship)))]
            :when (pos? (target-ship bank))]
      (swap! total inc))
    @total))

(defn count-attack-moves
  "For every world with red available make an attack move for each enemy ship not larger than the largest ship."
  [position]
  (let [player (:turn position)
        total (atom 0)]
    (doseq [world (vals (:worlds position))
            :when (colour-available-in-world? world "r" player)
            :when (pos? (count (player world)))
            :let [biggest-ship-size (reduce max 0 (map get-size-int (player world)))]
            enemy-ship (distinct (get world (other-player player)))
            :when (>= biggest-ship-size (get-size-int enemy-ship))]
      (swap! total inc))
    @total))

(defn count-move-moves
  "For every world with yellow available for each distinct ship for each world that the ship can navigate to."
  [position]
  (let [player (:turn position)
        total (atom 0)]
    (doseq [world (vals (:worlds position))
            :when (colour-available-in-world? world "y" player)
            ship (distinct (player world))
            target-world (vals (:worlds position))
            :when (can-navigate-between-worlds? position world target-world)]
      (swap! total inc))
    @total))

(defn count-discover-moves
  "For every world with yellow available for each distinct ship for each distinct pyramid available in the bank that it can navigate to."
  [position]
  (let [player (:turn position)
        bank (:bank position)
        total (atom 0)]
    (doseq [world (vals (:worlds position))
            :when (colour-available-in-world? world "y" player)
            ship (distinct (player world))
            target-world-star (keys bank)
            :when (can-discover? position world target-world-star)]
      (swap! total inc))
    @total))

(defn count-possible-moves
  "Count the number of possible moves quickly."
  [position]
  (+
    (count-build-moves position)
    (count-trade-moves position)
    (count-attack-moves position)
    (count-move-moves position)
    (count-discover-moves position)))

;; ## Code to locate a reasonable move to play

(defn score
  "Determine a score for how favourable the position is for the current player.
  Consider the number of legel moves each player has."
  [move position]
  (let [after (play-move position move)]
    (+
      (count-possible-moves after)
      (- (count-possible-moves (update-in after [:turn] other-player))))))

(defn find-best-move
  "Try to find the best move in a position."
  [position]
  (let [scores (for [move (find-all-possible-moves position)] [move (score move position)])
        dummy-val [:dummy -999999]
        best (reduce (fn [a b] (if (>= (second a) (second b)) a b)) dummy-val scores)]
    (first best)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
