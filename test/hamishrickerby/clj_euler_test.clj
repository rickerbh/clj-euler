;; ---------------------------------------------------------
;; hamishrickerby.clj-euler.-test
;;
;; Example unit tests for hamishrickerby.clj-euler
;;
;; - `deftest` - test a specific function
;; - `testing` logically group assertions within a function test
;; - `is` assertion:  expected value then function call
;; ---------------------------------------------------------


(ns hamishrickerby.clj-euler-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hamishrickerby.clj-euler :as clj-euler]))


(deftest application-test
  (testing "TODO: Start with a failing test, make it pass, then refactor"

    ;; TODO: fix greet function to pass test
    (is (= "hamishrickerby application developed by the secret engineering team"
           (clj-euler/greet)))

    ;; TODO: fix test by calling greet with {:team-name "Practicalli Engineering"}
    (is (= (clj-euler/greet "Practicalli Engineering")
           "hamishrickerby service developed by the Practicalli Engineering team"))))
