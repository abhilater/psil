(ns psil.components
  (:require [com.stuartsierra.component :as component]))

(defrecord PsilEngine [variable-lookup-map]
  component/Lifecycle
  (start [component]
    (println "Starting PsilEngine...")
    (assoc component :variable-lookup-map (atom {})))

  (stop [component]
    (println "Stopping PsilEngine...")
    (dissoc component :variable-lookup-map)))



