(ns psil.core-test
  (:require [clojure.test :refer :all]
            [psil.core :refer :all]))

(deftest run-src-test
  (testing "Psil run-src"
    ;; -
    (is (= 0 (run-src "(- 2 (* 1 2))")))
    (is (= -2 (run-src "(- 2)")))
    (is (= 1 (run-src "(- 2 1)")))
    (is (= 1 (run-src "(- 2 (+ 1))")))
    (is (= 2 (run-src "(* 2 (- 3 1 1))")))
    (is (= 4 (run-src "(bind a (- 2)) (bind b (- 2)) (* a b)")))

    ;; /
    (is (= 1 (run-src "(/ 2 (* 1 2))")))
    (is (= 1/2 (run-src "(/ 2)")))
    (is (= 2 (run-src "(/ 2 1)")))
    (is (= 4 (run-src "(* 2 (/ 4 2))")))
    (is (= 3/2 (run-src "(bind a (/ 3 1 2))")))
    (is (= 3 (run-src "(* (bind a (/ 3 1 2)) 2)")))

    ;; single
    (is (= 123 (run-src "123")))
    (is (= 0 (run-src "(+)")))
    (is (= 1 (run-src "(*)")))
    (is (= 1 (run-src "(+ 1)")))
    (is (= 1 (run-src "(* 1)")))

    ;; misc
    (is (= 2 (run-src "(* 1 1 1 1 2)")))
    (is (= 36 (run-src "(* 2 (+ 1 2 3) 3)")))
    (is (= 6 (run-src "(* 2 (+ 1 ) 3)")))
    (is (= 192 (run-src "(* 2 (+ 1 2) (* 2 (* 2 (* 2 (* 2 2)))))")))
    (is (= 192 (run-src "(* 2 (+ 1 2) (* 2 (* 2 (* 2 (* 2 2)))))")))
    (is (= 192 (run-src "(bind a (* 2 (+ 1 2) (* 2 (* 2 (* 2 (* 2 2))))))")))
    (is (= 200 (run-src "(bind a (* 2 (+ 1 2) (* 2 (* 2 (* 2 (* 2 2)))))) (+ 8 a)")))
    (is (= 10 (run-src "(bind a 1) (bind b 2) (+ 7 a b)")))
    (is (= 15 (run-src "(+ 1\n    (* 2 3)\n    (* 4 2))")))
    (is (= 12 (run-src "(bind radius 12)")))
    (is (= 100 (run-src "(bind length 10)\n(bind breadth 10)\n(* length breadth)")))

    ;; bind
    (is (= 100 (run-src "(bind length 10) (+ 1 2 3 4) (bind breadth 10)\n(* length breadth)")))
    (is (= 121 (run-src "(bind length 10)\n(bind breadth (+ length 1))\n(bind length 11)\n(* length breadth)")))
    (is (= 21 (run-src "(bind a 10)\n(bind b a)\n(bind a 11)\n(+ a b)")))

    ;; invalid
    (is (thrown? RuntimeException (run-src "(bind a 1) (bind b 2) (+ c a b)")))
    (is (thrown? RuntimeException (run-src "(bind a 1) (bind b 2) (+ c a b)")))
    (is (thrown? RuntimeException (run-src "(bind a a) (bind b 2) (+ c a b)")))
    (is (thrown? RuntimeException (run-src "()")))
    (is (thrown? RuntimeException (run-src "(1)")))
    (is (thrown? RuntimeException (run-src "a")))
    (is (thrown? RuntimeException (run-src "( a +")))
    (is (thrown? RuntimeException (run-src "( 1 + )")))
    (is (thrown? RuntimeException (run-src "(1 2 3 4 5)")))
    (is (thrown? RuntimeException (run-src "(+ 1) 2)")))
    (is (thrown? RuntimeException (run-src "(+ (* 1 2) + (* 3 4))")))

    ;; negative and fractional
    (is (thrown? RuntimeException (run-src "(+ -1 1")))
    (is (thrown? RuntimeException (run-src "(+ 1.2 1")))
    (is (thrown? RuntimeException (run-src "(+ 1/3 1")))
    (is (thrown? RuntimeException (run-src "(bind a 1/2")))


    ))
