(ns homeworldsai.core-test
  (:require [clojure.test :refer :all]
            [homeworldsai.core :refer :all]))

(deftest create-start
  (testing "Can create initial position"
    (is (not (nil? initial-position)))
    (is (= :player1 (:turn initial-position)))
    (is (not (nil? (:worlds initial-position))))
    (is (not (nil? (:bank initial-position))))))

(deftest sample-position-works
  (testing "Sample position sanity")
  (let [sp (create-sample-position)]
    (is (> 3 (get-in sp [:bank :g3])))))

(deftest build-move-logic
  (let [sample-position (create-sample-position)
        bank-before (:bank sample-position)]
    (testing "build y1 moon"
      (let [after-move (perform-build sample-position :y1 2)
            ships (get-in after-move [:worlds 2 :player1])
            bank-after (:bank after-move)]
        (is (not= bank-before bank-after))
        (is (zero? (:y1 bank-after)))
        (is (= :player1 (:turn after-move)))))
    (testing "build b1 combat"
      (let [after-move (perform-build (assoc sample-position :turn :player2) :b1 4)
            ships (get-in after-move [:worlds 4 :player2])
            bank (:bank after-move)]
        (is (some #{:b1} ships))))
    (testing "build g2 moon"
      (let [after-move (perform-build sample-position :g1 2)
            ships (get-in after-move [:worlds 2 :player1])
            bank (:bank after-move)]
        (is (some #{:g2} ships))
        (is (zero? (:g1 bank)))
        (is (= 2 (:g2 bank)))))))

(deftest trade-move-logic
  (let [sample-position (create-sample-position)]
    (testing "trade g3 y3 alice"
      (let [after-move (perform-trade sample-position :g3 :y3 0)
            ships (get-in after-move [:worlds 0 :player1])]
        (is (not (some #{:g3} ships)))
        (is (some #{:y3} ships))))))

(deftest attack-move-logic
  (let [sample-position (create-sample-position)]
    (testing "attack y3 b2 combat"
      (let [bank-before (:bank sample-position)
            after-move (perform-attack (assoc sample-position :turn :player2) :y3 :b2 4)
            bank-after (:bank after-move)
            player1-ships (get-in after-move [:worlds 4 :player1])
            player2-ships (get-in after-move [:worlds 4 :player2])] 
        (is (some #{:b2} player2-ships))
        (is (not (some #{:b2} player1-ships)))
        (is (= bank-before bank-after))))))

(deftest move-logic
  (let [sample-position (create-sample-position)]
    (testing "move g1 alice moon"
      (let [g1-ships-before (count (filter #(= :g1 %) (get-in sample-position [:worlds 2 :player1])))
            after-move (perform-move sample-position :g1 0 2)]
        (is (not (some #{:g1} (get-in after-move [:worlds 0 :player1]))))
        (is (= (inc g1-ships-before) (count (filter #(= :g1 %) (get-in after-move [:worlds 2 :player1])))))))
    (testing "moving the only ship away should return the star to the bank"
      (let [player2-ready (assoc-in sample-position [:turn] :player2)
            after-move (perform-move player2-ready :y1 3 2)]
        (is (= '[:y1] (get-in after-move [:worlds 2 :player2])))
        (is (nil? (get-in after-move [:worlds 3])))
        (is (= 1 (get-in after-move [:bank :g1])))))))

(deftest discover-logic
  (let [common-tests (fn [position]
                       (is (= 6 (:next-world position)))
                       (is (= '[:y3] (get-in position [:worlds 5 :stars])))
                       (is (= 1 (get-in position [:bank :y3]))))]
    (testing "discover g1 alice y3 new-name"
      (let [sample-position (create-sample-position)
            after-move (perform-discover sample-position :g1 0 :y3)]
        (common-tests after-move)
        (is (some #{:g1} (get-in after-move [:worlds 5 :player1])))
        (is (not (some #{:g1} (get-in after-move [:worlds 0 :player1]))))
        (is (not= (:bank sample-position) (:bank after-move)))))
    (testing "moving the only ship away should return the ship to the bank"
      (let [player2-ready (assoc (create-sample-position) :turn :player2)
            after-move (perform-discover player2-ready :y1 3 :y3)]
        (common-tests after-move)
        (is (= 1 (get-in after-move [:bank :g1])))
        (is (nil? (get-in after-move [:worlds 3])))))))

(deftest sacrifice-logic
  (let [sample-position (create-sample-position)]
    (testing "sacrificing a ship returns it to the bank"
      (let [after-move (perform-sacrifice sample-position :g1 2)]
        (is (= 1 (get-in after-move [:bank :g1])))
        (is (= '[:y1] (get-in after-move [:worlds 2 :player1])))))
    (testing "sacrificing the last ship in a star also returns the star to the bank"
      (let [after-move (perform-sacrifice (assoc sample-position :turn :player2) :y1 3)]
        (is (= 1 (get-in after-move [:bank :g1])))
        (is (= 2 (get-in after-move [:bank :y1])))))))

(deftest catastrophe-logic
  (let [sample-position (create-sample-position)
        too-many-ships (-> sample-position
                           (perform-build :y1 4)
                           (perform-build :y1 4)
                           (perform-build :y1 4))]
    (testing "Destroy four ships of one player"
      (let [after-move (perform-catastrophe too-many-ships "y" 4)]
        (is (empty? (get-in after-move [:worlds 4 :player1])))
        (is (= 1 (get-in after-move [:bank :y1])))
        (is (= 2 (get-in after-move [:bank :y2])))
        (is (= 2 (get-in after-move [:bank :y3])))))
    (testing "Destroy four ships with another ship left behind"
      (let [one-extra-ship (perform-move too-many-ships :g1 2 4)
            after-move (perform-catastrophe one-extra-ship "y" 4)]
        (is (= [:g1] (get-in after-move [:worlds 4 :player1])))
        (is (= 2 (get-in after-move [:bank :y3])))))
    (testing "Destroy four ships from two players"
      (let [two-ships-each (-> sample-position
                               (perform-build :y1 4)
                               (assoc :turn :player2)
                               (perform-move :y3 2 4)
                               (perform-build :y3 4)
                               (assoc :turn :player1))
            after-move (perform-catastrophe two-ships-each "y" 4)]
        (is (= [:b3] (get-in after-move [:worlds 4 :player2])))))
    (testing "Destroy four ships and return star to bank"
      (let [two-ships-each (-> sample-position
                               (perform-build :y1 4)
                               (assoc :turn :player2)
                               (perform-move :y3 2 4)
                               (perform-build :y3 4)
                               (perform-move :b3 4 2)
                               (assoc :turn :player1))
            after-move (perform-catastrophe two-ships-each "y" 4)]
        (is (nil? (get-in after-move [:worlds 4])) "World removed")
        (is (= 3 (get-in after-move [:bank :r1])))) "Star returned to bank")
    (testing "Destroy three ships and a star"
      (let [three-ships-and-star (-> sample-position
                                     (assoc :turn :player2)
                                     (perform-trade :b3 :r3 4)
                                     (perform-build :r3 4)
                                     (perform-build :r3 4))
            after-move (perform-catastrophe three-ships-and-star "r" 4)]
        (is (nil? (get-in after-move [:worlds 4])) "World removed")
        (is (= 3 (get-in after-move [:bank :r1])))) "Star returned to bank")
    (testing "Destroy three ships and a homeworld star"
      (let [homeworld-and-three-ships (-> sample-position
                                          (perform-trade :g1 :y1 0)
                                          (perform-build :y1 0)
                                          (perform-build :y1 0))
            after-move (perform-catastrophe homeworld-and-three-ships "y" 0)]
        (is (= [:b1] (get-in after-move [:worlds 0 :stars])))))))
