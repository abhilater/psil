(ns psil.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [com.stuartsierra.component :as component]
            [psil.components :as comps])
  (:gen-class))

(defn bind-fn [variable-lookup-map args]
  (swap! variable-lookup-map assoc (first args) (second args))
  (@variable-lookup-map (first args)))


(defn +-fn [_ args]
  (apply + args))


(defn *-fn [_ args]
  (apply * args))


(def symbol-map {"+" +-fn "*" *-fn "bind" bind-fn})


(defn xsymbol?
  "(xsymbol? \"+\")
  => true
  (xsymbol? \"bind\")
  => true"
  [s]
  (contains?
    (set (keys symbol-map)) s))

(defn xnumber?
  "(xnumber? \"123\")
  => true
  (xnumber? \"12a3\")
  => false
  (xnumber? \"-1\")
  => false"
  [s]
  (try (>= (Integer/parseInt s) 0)
       (catch NumberFormatException nfe false)))


(defn variable?
  "(variable? \"a2a\")
  => false
  (variable? \"aa\")
  => true
  (variable? \"a_a\")
  => false"
  [s]
  (->> s
       (every? #(or
                  (and (>= (int %) 65) (<= (int %) 90))
                  (and (>= (int %) 97) (<= (int %) 122))))))

(defn raise []
  (throw (RuntimeException. "Invalid Program")))


(defn not-emp [stack]
  (not (.isEmpty stack)))


(defn strip-line-breaks-tabs [x]
  (s/replace x #"[\n\t\r]" " "))


(defn tokenize-s-expr-str [s-exp-str]
  (s/split (.trim s-exp-str) #"\s+"))


(defn tokenize-source [src]
  (filter (comp not empty?) (s/split (s/replace (s/replace src #"\(" " ( ") #"\)" " ) ") #"\s+")))


(defn parse-file
  "Parses input filename to produce tokenized code sequence
  (parse-file \"p7.psil\")
  => (\"(\" \"bind\" \"length\" \"10\" \")\" \"(\" \"bind\" \"breadth\" \"10\" \")\" \"(\" \"*\" \"length\" \"breadth\" \")\")\n
  "
  [file]
  (let [prog-str (slurp (io/resource file))]
    (->> (strip-line-breaks-tabs prog-str)
         tokenize-source)))


(defn parse-source
  "Processes token sequence to segregate S-Expressions, also checks S-Exp
  syntacs and raise error if invalid. Produces a seq of S-Exp strings

  (parse-source (parse-file \"p7.psil\"))
  => [\"( bind length 10 )\" \"( bind breadth 10 )\" \"( * length breadth )\"]"
  [source]
  (let [stack (java.util.Stack.)
        sb (StringBuilder.)
        result (java.util.ArrayList.)
        pop-till-balanced (fn [s]
                            (if (.isEmpty s) (raise))
                            (while (and (not-emp s) (not (= (.peek s) "(")))
                              (.pop s))
                            (if (not-emp s) (.pop s) (raise)))]
    (doseq [token source]
      (cond
        (= token "(") (do (.append sb (str token " ")) (.push stack token))
        (or (xsymbol? token)
            (xnumber? token)
            (variable? token)) (.append sb (str token " "))
        (= token ")") (do (.append sb (str token " ")) (pop-till-balanced stack))
        :else (raise))
      (if (.isEmpty stack)
        (do (.add result (.trim (.toString sb)))
            (.setLength sb 0))))

    (if (not-emp stack)
      (if (not (xnumber? (.peek stack)))
        (raise)
        (.add result (.pop stack)))
      result)))


(defn handle-number [stack number]
  (if (and (not-emp stack)
           (or (= (.peek stack) "(")
               (= (.peek stack) ")")))
    (raise)
    (if (.isEmpty stack)
      (raise)
      (.push stack (Integer/parseInt number)))))


(defn handle-symb [stack symb]
  (if (.isEmpty stack)
    (raise)
    (if (not= (.peek stack) "(")
      (raise)
      (.push stack symb))))


(defn resolve-var [variable-lookup-map stack var]
  (if (= (.peek stack) "bind")
    var
    (if (contains? @variable-lookup-map var)
      (@variable-lookup-map var)
      (raise))))


(defn handle-var [variable-lookup-map stack var]
  (if (not-emp stack)
    (if (or (= (.peek stack) "(")
            (= (.peek stack) ")"))
      (raise)
      (.push stack (resolve-var variable-lookup-map stack var)))
    (raise)))


(defn handle-close-brack [variable-lookup-map stack]
  (let [operands (java.util.ArrayList.)]
    (if (.isEmpty stack) (raise))
    (while (and (not-emp stack) (not (= (.peek stack) "(")))
      (.add operands (.pop stack)))
    (if (not-emp stack)
      (do (.pop stack)
          (if (empty? operands) (raise))
          (let [ops-reversed (reverse operands)]
            (.push stack
                   ((symbol-map (first ops-reversed)) variable-lookup-map (rest ops-reversed)))))
      (raise))))


(defn eval-s-exp [variable-lookup-map s-exp-str]
  "Evaluates a single S-Exp string

  (eval-s-exp \"123\")
  => 123"
  (let [s (java.util.Stack.)
        tokens (tokenize-s-expr-str s-exp-str)
        cnt (count tokens)]
    (cond
      (< cnt 1) (raise)
      (= cnt 1) (if (xnumber? (first tokens))
                  (.push s (Integer/parseInt (first tokens)))
                  (raise))
      :else (doseq [token tokens]
              (cond
                (= token "(") (.push s token)
                (xsymbol? token) (handle-symb s token)
                (xnumber? token) (handle-number s token)
                (variable? token) (handle-var variable-lookup-map s token)
                (= token ")") (handle-close-brack variable-lookup-map s)
                :else (raise)
                )))
    (.pop s)))


(defn eval-program
  "Evaluates the sequence of S-Exps strings and returns the value of the last one"
  [variable-lookup-map s-exp-str-seq]
  (doseq [s-exp-str (drop-last s-exp-str-seq)]
    (eval-s-exp variable-lookup-map s-exp-str))
  (eval-s-exp variable-lookup-map (last s-exp-str-seq)))


(defn start-system []
  (let [system (component/system-map
                 :psil-engine (comps/map->PsilEngine {}))]
    (component/start-system system)))
(def system (start-system))
(defn get-lookup-table []
  (:variable-lookup-map (:psil-engine system)))

(defn run [file]
  (->> file
       parse-file
       parse-source
       (eval-program (get-lookup-table))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run (first args)))