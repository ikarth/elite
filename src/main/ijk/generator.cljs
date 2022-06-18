(ns ijk.generator
  (:require
   [ijk.elite :as elite]
   [ijk.elite-grammar :as egrammar]
   [datascript.core :as d]
   ;;[clojure.spec.alpha :as spec]
   ;;[clojure.edn :as edn]
   ;;[clojure.string :as cstring]
   ;;[clojure.math]
   ;;[grotesque.core :as grot]
   ;;["js-xxhash" :as xx :refer (xxHash32)]

  ))


(defn log-db
  "Log a complete listing of the entities in the provided `db` to the console."
  [db]
  (d/q '[:find ?any ?obj :where [?obj :type ?any]] @db)) ;todo: log to console...




(defn positions
  "Find the index of an item in a vector
  https://stackoverflow.com/a/4831131/5562922"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

;; (def database-records
;;   {:elite {:db-conn }})

(defonce current-database
  (atom {:db-conn nil
         :db-schema {}
         }))


(defn fetch-internal-view
  "Returns the internal database. Intended mostly for debugging visualization."
  []
  (if-let [db-conn (get @current-database :db-conn)]
    (vec (map (fn [dat]
                (let [[e a v tx add] dat]
                  [e a v tx add])) (d/datoms @db-conn :eavt)))
    (println "Database connection missing when trying to fetch a new view.")))

(def elite-schema {:seed/planet            {:db/cardinality :db.cardinality/one   :db/unique :db.unique/identity}
                   :seed/description       {:db/cardinality :db.cardinality/one}
                   :seed/galaxy            {:db/cardinality :db.cardinality/one}
                   :planet/economy-type    {:db/cardinality :db.cardinality/one}
                   :planet/species         {:db/cardinality :db.cardinality/one}
                   :planet/government-type {:db/cardinality :db.cardinality/one}
                   :planet/name-length     {:db/cardinality :db.cardinality/one}
                   :planet/partial-name    {:db/cardinality :db.cardinality/one}
                   :planet/name            {:db/cardinality :db.cardinality/one}
                   :planet/description     {:db/cardinality :db.cardinality/one}}
  )



(def elite-db-conn (d/create-conn elite-schema))
(d/transact! elite-db-conn [{:seed/galaxy (elite/make-seed [0x5A4A 0x0248 0xB753])
                             :galaxy/index 0}
                            {:planet/galaxy 0
                             :planet/index 0
                             :planet/seed (elite/make-seed [0x5A4A 0x0248 0xB753])}
                            ])
      
(comment 
(swap! current-database assoc-in [:db-conn]
       elite-db-conn)
(swap! current-database assoc-in [:db-schema]
       elite-schema))

(def operations
  [{:name "make-planet"
    :exec
    (fn [galaxy-seed galaxy-index previous-max-planet]
      (elite/make-planet galaxy-seed galaxy-index (+ 1 previous-max-planet)))
    :query
    '[:find ?galaxy-seed ?galaxy-index (max ?planet-index)
      :in $ %
      :where
      [?galaxy-id :seed/galaxy ?galaxy-seed]
      [?galaxy-id :galaxy/index ?galaxy-index]
      [?planet-id :planet/galaxy ?galaxy-index]
      [?planet-id :planet/index-number ?planet-index]
      [(= 0 ?galaxy-index)]]
    :input [:seed/galaxy :planet/index-number]}
   ;; {:name "planet-government"}
   ;; {:name "planet-economy"}
   ;; {:name "planet-tech-level"}
   ;; {:name "planet-population-size"}
   ;; {:name "planet-productivity"}
   ;; {:name "planet-name"}
   ;; {:name "planet-species"}
   ;; {:name "planet-description"}
   ])


;; print database
(defn print-database []
  (println "----------")
  (doseq [dat (vec (map (fn [dat]
                          (let [[e a v tx add] dat]
                            [e a v tx add])) (d/datoms @elite-db-conn :eavt)))]
    (println dat)))


;; check ops for which ones can be executed...

(defn can-perform-operation [prime-op]
  (let [q-map
        ;;(let [prime-op (nth operations 0)])
        (if-let [op-query (get prime-op :query false)]
          (map (fn [v] {:op prime-op :parameters v}) 
               (d/q (:query prime-op) @elite-db-conn nil))
          {:op prime-op :parameters nil})]
    (if (map? q-map) [q-map] q-map)))

(apply concat (map can-perform-operation operations))

(defn assess-operations [ops]
  (apply concat (map can-perform-operation ops)))


(defn execute-op! [chosen-op]
  (d/transact!
   elite-db-conn
   (let [prime-op (get-in chosen-op [:op])]
     (apply
      (:exec prime-op)
      (get-in chosen-op [:parameters])))))


(defn choose-op
  "Choose an op from the valid subset of the global operations.
  TODO: currently nondetermanistic, make determanistic"
  []
  (rand-nth (assess-operations operations)))

(execute-op! (choose-op))
       


