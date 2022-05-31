(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   ;;[clojure.edn :as edn]
   ;;[clojure.string :as cstring]
   ;;[grotesque.core :as grot]
   ;;["js-xxhash" :as xx :refer (xxHash32)]

  ))


(egrammar/goat-soup 0 "Lave")


(defn twist-seed
  "Takes a random seed and twists it using Elite's 'tribonocci' method."
  [old-seed]
  old-seed
  )

(defn run []
  (.log js/console "Hello")
  (println "World"))
