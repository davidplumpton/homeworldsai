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
  (testing "build y1"
    (let [sample-position (create-sample-position)
          after (build-move sample-position :y1 2)
          ships (get-in after [:worlds 2 :player1])
          bank (:bank after)]
      (is (not (nil? bank)))
      (is (zero? (:y1 bank)))
      )
    )
  )
