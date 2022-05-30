(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   [clojure.edn :as edn]
   [clojure.string :as cstring]
   [grotesque.core :as grot]
   ["js-xxhash" :as xx :refer (xxHash32)]

  ))



(defn run []
  (.log js/console "Hello")
  (println "World"))