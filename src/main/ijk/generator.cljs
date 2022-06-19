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

(def elite-schema {:planet/seed            {:db/cardinality :db.cardinality/one}
                   :description/seed       {:db/cardinality :db.cardinality/one}
                   :galaxy/seed            {:db/cardinality :db.cardinality/one}
                   :galaxy/index            {:db/cardinality :db.cardinality/one   :db/unique :db.unique/identity}
                   :planet/index            {:db/cardinality :db.cardinality/one   :db/unique :db.unique/identity}
                   :planet/economy-type    {:db/cardinality :db.cardinality/one}
                   
                   :planet/species         {:db/cardinality :db.cardinality/one}
                   :planet/government-type {:db/cardinality :db.cardinality/one}
                   :planet/name-length     {:db/cardinality :db.cardinality/one}
                   :planet/partial-name    {:db/cardinality :db.cardinality/one}
                   :planet/name            {:db/cardinality :db.cardinality/one}
                   :planet/description     {:db/cardinality :db.cardinality/one}}
  )



(def elite-db-conn (d/create-conn elite-schema))
(d/transact! elite-db-conn [{:galaxy/seed (elite/make-seed [0x5A4A 0x0248 0xB753])
                             :galaxy/index 0}
                            {:planet/galaxy 0
                             :planet/index 0
                             :planet/seed (elite/make-seed [0x5A4A 0x0248 0xB753])}
                            ])

(choose-op)
(execute-op! (choose-op))
(print-database)

(d/q
 '[:find ?planet-index ?allowed-galaxy ;; ?galaxy-count
   ;;:with ?planet-index
   :in $ [?allowed-galaxy ...]
   :where
   [?galaxy-id :galaxy/seed ?galaxy-seed]
   [?galaxy-id :galaxy/index ?galaxy-index]
   [(= ?galaxy-index ?allowed-galaxy)]
   [?planet-id :planet/galaxy ?galaxy-index]
   [?planet-id :planet/index ?planet-index]
   ;;[(ijk.generator/galaxy-planet-count ?galaxy-index) ?galaxy-count]
      
    ;;[(max ?planet-index) ?max-planet-index]
    ;;[(< ?max-planet-index 4)]
    
   ]
 @elite-db-conn
 [1 2])

(galaxy-planet-count 0)


(comment 
(swap! current-database assoc-in [:db-conn]
       elite-db-conn)
(swap! current-database assoc-in [:db-schema]
       elite-schema))

(def limit-to-galaxy-planet-count 4 ;;256
  )
(def limit-to-galaxy-count 5)

(defn get-new-planet-index [db galaxy-index]
  (let [new-index (d/q
                   '[:find (max ?planet-index) .
               :where
                [?planet-id :planet/galaxy ?galaxy-index]
                [?planet-id :planet/index ?planet-index]
                ]
             @db)
        ]
    (+ 1 new-index)
    ))

(defn make-planet
  "Returns the seed for the planet at planet-index."
  [galaxy-seed galaxy-index planet-index]
  [{:db/id -1
    :planet/galaxy galaxy-index
    :planet/index planet-index
    :planet/seed
    (let []
      (if (> planet-index 0)
        (last (take (+ 1 planet-index) (iterate elite/twist-to-next-planet galaxy-seed)))
        galaxy-seed))}])


(defn galaxy-planet-count [galaxy-index]
  (d/q
 '[:find (max ?planet-index) .
   ;;:with ?planet-index
   :in $ ?galaxy-index
   :where
   [?galaxy-id :galaxy/index ?galaxy-index]
   [?planet-id :planet/galaxy ?galaxy-index]
   [?planet-id :planet/index ?planet-index]
   
   ]
 @elite-db-conn galaxy-index)
  )

(galaxy-planet-count 0)

(filter #(< (galaxy-planet-count %) limit-to-galaxy-planet-count
        
        ) (range limit-to-galaxy-count))

(def operations
  [{:name "make-planet"
    :exec
    (fn [galaxy-seed galaxy-index]
      (let [new-planet-index (get-new-planet-index elite-db-conn galaxy-index)
            new-planet (make-planet galaxy-seed galaxy-index new-planet-index)]
        ;;(println new-planet)
        new-planet
        ))
    ;;:exclude
    :query-data
    (fn []
      (filter #(< (galaxy-planet-count %)
                  limit-to-galaxy-planet-count)
              (range limit-to-galaxy-count)))
    :query
    '[:find ?galaxy-seed ?galaxy-index ;;(max ?planet-index)
      :in $  [?allowed-galaxy ...]
      :where
      [?galaxy-id :galaxy/seed ?galaxy-seed]
      [?galaxy-id :galaxy/index ?galaxy-index]
      [?planet-id :planet/galaxy ?galaxy-index]
      [?planet-id :planet/index ?planet-index]
      [(= ?galaxy-index ?allowed-galaxy)]
      ;;[(galaxy-planet-count ?galaxy-index) ?galaxy-count]
      ;; [(= 0 ?galaxy-index)]
      ;;[(> (max ?planet-index) 4)]
      ;;[(> ?planet-index limit-to-galaxy-planet-count)]
      ;;[(> (max ?planet-index) 6)] ;; limit galaxies to 256 planets, like the original...
      ;; (not-join [?planet-index]
                ;; [_ :planet/index (+ ?planet-index 1)])
      ]    
    ;;:input [:galaxy/seed :planet/index]
    }
   {:name "planet-government"
    :exec (fn [planet-seed planet-reference]
            [{:db/id planet-reference :planet/government-type (elite/planet-government planet-seed)}])
    :query
    '[:find ?planet-seed ?ungoverned-planet
      :in $ %
      :where
      [?ungoverned-planet :planet/seed ?planet-seed]
      (not-join [?ungoverned-planet]
                [?ungoverned-planet :planet/government-type _] )
      ]
    ;;:input [:planet/seed :planet/reference ]
    }
   
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
                            [e a v tx add]))
                        (d/datoms @elite-db-conn :eavt)))]
    (if (some #{(nth dat 1)} '(:galaxy/seed :planet/seed))
      (let [[e a v tx add] dat]
        (println [e a
                  (elite/get-seed-bytes v)
                  tx
                  add]))
      (println dat))

    ))


;; check ops for which ones can be executed...

(defn can-perform-operation [prime-op]
  ;;(println "\t\t" prime-op)
  (let [q-map
        ;;(let [prime-op (nth operations 0)])
        (if-let [op-query (get prime-op :query false)
                 
                 ]
          (let [op-data-fn (get prime-op :query-data nil)
                 op-data (if (fn? op-data-fn)
                           (op-data-fn)
                           nil)]
            (map (fn [v] {:op prime-op :data op-data :parameters v}) 
                 (d/q (:query prime-op) @elite-db-conn op-data)))
          {:op prime-op :parameters nil})]
    (if (map? q-map) [q-map] q-map)))

;;(apply concat (map can-perform-operation operations))

(defn assess-operations [ops]
  (apply concat (map can-perform-operation ops)))

(defn report-problem [description]
  (println (apply str description))
  )


(defn execute-op! [chosen-op]
  (if (empty? chosen-op)
    (report-problem "No valid options")
    (let []
      (println (get-in chosen-op [:op :name])
               ;;(apply str (get-in chosen-op [:parameters]))
               )
      (d/transact!
       elite-db-conn
       (let [prime-op (get-in chosen-op [:op])
             parameters (into [elite-db-conn] (get-in chosen-op [:parameters]))
             result (apply
          (:exec prime-op)
          (get-in chosen-op [:parameters])
          )]
         ;;(println (get-in chosen-op [:parameters]))
         ;;(println result)
         result)))))


(d/transact! elite-db-conn
             [{:db/id -1
               :test 2}]
             )

(defn choose-op
  "Choose an op from the valid subset of the global operations.
  TODO: currently nondetermanistic, make determanistic"
  []
  (let [valid-options (assess-operations operations)]
    (if (empty? valid-options)
      valid-options
      (rand-nth valid-options))))


(print-database)
(assess-operations operations)
(choose-op)
(execute-op! (choose-op))
(print-database)
       


