(ns n2n-assembler.core-test
  (:require [clojure.test :refer :all]
            [n2n-assembler.core :refer :all]
            [clojure.string :as str]))

(defn- to-list
  [lines]
  (str/split lines #"\n"))

(deftest test-max
  (let [input  (slurp "resources/max/Max.asm")
        output (to-list (slurp "resources/max/Max.hack"))]
    (is (= (assembler input) output))))

(deftest test-add
  (let [input  (slurp "resources/add/Add.asm")
        output (to-list (slurp "resources/add/Add.hack"))]
    (is (= (assembler input) output))))
