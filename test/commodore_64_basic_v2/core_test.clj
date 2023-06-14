(ns commodore-64-basic-v2.core-test
  (:require [clojure.test :refer :all]
            [commodore-64-basic-v2.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))) 
  (testing "Soy la segunda de a-test." 
    (is (= 2 2))
    (is (= 2 2))
    (is (= 2 2))
    (is (= 2 2))))


(deftest b-test
  (testing "Soy 2."
    (is (= 2 2))))