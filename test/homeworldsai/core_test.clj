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
  (let [sample-position (create-sample-position) ]
    (testing "build y1 moon"
      (let [after (perform-build sample-position :y1 2)
            ships (get-in after [:worlds 2 :player1])
            bank (:bank after)]
        (is (zero? (:y1 bank)))
        (is (= :player1 (:turn after)))
        ))
    (testing "build b1 combat"
      (let [after (perform-build (assoc sample-position :turn :player2) :b1 4)
            ships (get-in after [:worlds 4 :player2])
            bank (:bank after)]
        (is (some #{:b1} ships))))
    (testing "build g2 moon"
      (let [after (perform-build sample-position :g1 2)
            ships (get-in after [:worlds 2 :player1])
            bank (:bank after) ]
        (is (some #{:g2} ships))
        (is (zero? (:g1 bank)))
        (is (= 2 (:g2 bank)))))))

(deftest trade-move-logic
  (let [sample-position (create-sample-position)]
    (testing "trade g3 y3 alice"
      (let [after (perform-trade sample-position :g3 :y3 0)
            ships (get-in after [:worlds 0 :player1])]
        (is (not (some #{:g3} ships)))
        (is (some #{:y3} ships))
        )
      )
    )
  )
