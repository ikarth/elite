(ns ijk.generator
  (:require-macros
   [ijk.generator]
   [cljs.repl])
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


;(elite/make-seed [0x5A4A 0x0248 0xB753]) 0 0


(defn clause-out [attr]
  `[(~'not-join [~'?entity]
     [~'?entity ~attr ~'_])])

(defn clause-out-vector [attrs]
  (vec (apply concat
              (map clause-out attrs))))

(defn clause-in [[attr-key attr-sym]]
  `[~'?entity ~attr-key ~(symbol attr-sym)])

(defn clause-in-vector [attrs]
  (vec (map clause-in attrs)))


(defn make-q-syms [key-data]
  (take (count key-data)
        (map
         #(symbol (keyword (str "?attr-" %)))
         (iterate inc 1))))

(defn make-q-syms-str [key-data]
  (take (count key-data)
        (map
         #(str "?attr-" %)
         (iterate inc 1))))


(defn make-query [in-keys out-keys]
  (let [q-syms (make-q-syms in-keys)]
    (vec
     (concat
      `[:find ~'?entity ~@q-syms]
      '[:in $ %]
      '[:where]
      ;;`[[~'?entity ~(first q-syms)]]
      (clause-in-vector (zipmap in-keys q-syms))
      (clause-out-vector out-keys)))))

(defn generate-attribute
  [{:keys [name input output exec-fn] :as op-data}]
  {:name name
   :input input
   :exec
   (fn
     ;;"Auto-generated function to take the given input (of the specified types) and run the designated function to produce the specified output. Note that the exec-function is expected to return a vector that will be zipped against the vector of output types to produce the map for the transaction. "
     [& exec-input]
     (let [result (apply exec-fn (rest exec-input))
           ;; Note that if the exec-fn returns anything other than a sequence,
           ;; we need to wrap it in a vector so that it can match up with the
           ;; vector of labels for the attributes.
           ;; However, if it is a vector already we need to keep it that way...
           result-vec
           (cond
             (seq? result)
             (vec result)
             (vector? result)
             (vec result)
             true
             [result]
             )
           combined (merge {:db/id (first exec-input)}
                           (zipmap output result-vec))]
       [combined]))
   :query
   (make-query input output)})




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

(defn reset-database! []
  (d/reset-conn! elite-db-conn (d/empty-db elite-schema))
  (d/transact! elite-db-conn [{:galaxy/seed (elite/make-seed [0x5A4A 0x0248 0xB753])
                             :galaxy/index 0}
                            {:planet/galaxy 0
                             :planet/index 0
                             :planet/seed (elite/make-seed [0x5A4A 0x0248 0xB753])}
                            ]))

;;(reset-database!)
;;(print elite-db-conn)



(def limit-to-galaxy-planet-count 4;;256
  )
(def limit-to-galaxy-count 8)

(defn get-new-planet-index [db galaxy-index]
  (let [new-index (d/q
                   '[:find (max ?planet-index) .
               :where
                [?planet-id :planet/galaxy ?galaxy-index]
                [?planet-id :planet/index ?planet-index]
                ]
             @db)]
    (+ 1 new-index)))

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
   [?planet-id :planet/index ?planet-index]]
 @elite-db-conn galaxy-index))

(defn nearby-planets
  [galaxy-index planet-index]
  
  )

(defn survey-attributes
  [galaxy-index attribute-name]
  (d/q
   '[:find (count ?planet-attribute) .
     :in $ ?galaxy-index ?attribute-name
     :where
     [?galaxy-id :galaxy/index ?galaxy-index]
     [?planet-id :planet/galaxy ?galaxy-index]
     [?planet-id :planet/index ?planet-index]
     [?planet-id ?attribute-name ?planet-attribute]
     ]
   @elite-db-conn galaxy-index attribute-name))

(def operations
  [{:name "make-planet"
    :exec
    (fn [galaxy-seed galaxy-index]
      (let [new-planet-index (get-new-planet-index elite-db-conn galaxy-index)
            new-planet (make-planet galaxy-seed galaxy-index new-planet-index)]
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
      ]}
   ;; {:name "calculate-planet-hub-count"
   ;;  :exec
   ;;  (fn [])
   ;;  :query-data
   ;;  (fn []
   ;;    (filter #(= (galaxy-planet-count %)
   ;;                limit-to-galaxy-planet-count)
   ;;            (range limit-to-galaxy-count)))
   ;;  :query
   ;;  '[:find ?galaxy-index
   ;;    :in $ [?allowed-galaxy ...]
   ;;    :where
   ;;    [(= ?galaxy-index ?allowed-galaxy)]
      
   ;;    ]
   ;;  }
   {:name "make-planet-name"
    :exec
    (fn [p-id p-seed n-length partial]
      ;;(println [p-id p-seed n-length partial])
      (let [output (elite/generate-name [p-seed n-length partial])]
        ;;(println output)
        (cond (string? output)
              [{:db/id p-id
                ;;:planet/token-seed 
                :planet/name output}]
          true
          [{:db/id p-id
            :planet/token-seed (nth output 0)
            :planet/name-partial (nth output 2)
            :planet/name-length-remaining (nth output 1)
            }])))
    :query
    '[:find ?unnamed-planet ?token-seed ?name-length-remaining ?partial-name
      :in $ %
      :where
      [?unnamed-planet :planet/name-partial ?partial-name]
      [?unnamed-planet :planet/token-seed ?token-seed]
      [?unnamed-planet :planet/name-length-remaining ?name-length-remaining]
      [?unnamed-planet :planet/seed ?planet-seed]
      (not-join [?unnamed-planet]
                [?unnamed-planet :planet/name _] )
      ]}
   {:name "planet-name-start"
    :exec
    (fn [planet-seed db-id]
      (let [[token-seed name-length-remaining planet-name] (elite/generate-name-start planet-seed)]
        [{:db/id db-id
          :planet/name-partial planet-name
          :planet/token-seed token-seed
          :planet/name-length-remaining name-length-remaining}]))
    :query
    '[:find ?planet-seed ?unnamed-planet
      :in $ %
      :where
      [?unnamed-planet :planet/seed ?planet-seed]
      (not-join [?unnamed-planet]
                [?unnamed-planet :planet/name-partial _] )
      (not-join [?unnamed-planet]
                [?unnamed-planet :planet/name _] )]}
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
      ]}
   (generate-attribute
    {:name "planet-economy"
     :input [:planet/seed :planet/government-type]
     :output [:planet/economy-type :planet/economy-prosperity]
     :exec-fn elite/planet-economy})
   (generate-attribute
    {:name "planet-tech-level"
     :input [:planet/seed :planet/economy-prosperity :planet/government-type]
     :output [:planet/tech-level]
     :exec-fn elite/planet-tech-level-from-prosperity})
   (generate-attribute
    {:name "planet-population-size"
     :input [:planet/tech-level :planet/economy-prosperity :planet/government-type]
     :output [:planet/population-size]
     :exec-fn ;;(fn [entity-id & rest] (apply elite/planet-population-size rest))
     elite/planet-population-size
     })
   (generate-attribute
    {:name "planet-productivity"
     :input [:planet/economy-prosperity :planet/government-type :planet/population-size]
     :output [:planet/economic-productivity]
     :exec-fn ;;(fn [entity-id & rest ] (apply elite/planet-productivity rest))
     elite/planet-productivity})
   (generate-attribute
    {:name "planet-species"
     :input [:planet/seed]
     :output [:planet/species]
     :exec-fn elite/planet-species
     })
   (generate-attribute
    {:name "planet-description"
     :exec-fn elite/planet-goat-soup
     :input [:planet/seed :planet/name]
     :output [:planet/description]
     })
   (generate-attribute
    {:name "galactic-x"
     :exec-fn elite/galactic-x
     :input [:planet/seed]
     :output [:planet/galactic-x]})
   (generate-attribute
    {:name "galactic-y"
     :exec-fn elite/galactic-y
     :input [:planet/seed]
     :output [:planet/galactic-y]})
      (generate-attribute
    {:name "size-on-map"
     :exec-fn elite/size-on-map
     :input [:planet/seed]
     :output [:planet/size-on-map]})
   ])


(def export-translation
  {;;:planet/index #(%)
   :planet/seed  #(elite/get-seed-bytes %)
   ;;:planet/name  #(%)
   ;;:planet/tech-level #(%)
   :planet/economy-type #(get (zipmap (range) [:industrial :agricultural]) % "Unknown")
   :planet/economy-prosperity #(get {0 :rich
                                     1 :average
                                     2 :poor
                                     3 :mainly
                                     4 :mainly
                                     5 :rich
                                     6 :average
                                     7 :poor} % "Unknown")
   :planet/government-type #(elite/government-name %)
   :planet/population-size #(str (* % 0.1 )" Billion")
   :planet/economic-productivity #(str % " M CR")
   :planet/species #(str %)
   :planet/description #(str %)
   })

(defn export-entity
  "Input: a vector of the data linked to an entity. Prints the data in a readable format. Returns nil."
  [entity]
  (println "---")
  (doseq [[e a v tx add] (vec
                          (map
                           (fn [dat]
                             (let [[e a v tx add] dat]
                               [e a v tx add]))
                           entity))]
    ;;(println [e a v tx add])
    (println (str a ":\t" ((get export-translation a (fn [x] x)) v)))
    ))

;;((get export-translation :planet/economy-type) 0)

(defn fetch-attribute [planet attribute]
   (let [attr (get planet attribute nil)
         value (if (number? attr)
                 ((get export-translation attribute (fn [x] x))
                  attr)
                 attr)]
     (if (keyword? value)
       (name value)
       value)))

(defn export-planet [planet-data]
  ;;(println planet-data)
  (let [planet (into {}
                     (map
                      (fn [dat]
                        ;;(println dat)
                        (let [[e a v t add] dat]
                          [a v]))
                      planet-data))
        planet-index (get planet :planet/index nil)]
    ;;(println planet)
    (if (not (nil? planet-index))
      ;;(println ">")
      (str
       "### "
       (get planet :planet/name "UNNAMED") " (" planet-index ")" " ###\n"
       "Government:\t" (fetch-attribute planet :planet/government-type) "\n"
       "Economy:\t" (fetch-attribute planet :planet/economy-prosperity) " "
       (fetch-attribute planet :planet/economy-type) "\n"
       "Tech Level:\t" (fetch-attribute planet :planet/tech-level)"\n"
       "Population:\t" (fetch-attribute planet :planet/population-size) "\n"
       "Productivty:\t" (fetch-attribute planet :planet/economic-productivity) "\n"
       "Species:\t" (fetch-attribute planet :planet/species) "\n"
       ;;"Description:\t" (fetch-attribute planet :planet/description) ""
       "Location:\t" (fetch-attribute planet :planet/galactic-x) "x " (fetch-attribute planet :planet/galactic-y) "y\n"
       ))))

(defn export-galaxy []
  (let [data (d/datoms @elite-db-conn :eavt)
        by-entity (group-by first data)]
    (doseq [ent by-entity]
      ;;(export-entity (second ent))
      (println (export-planet (second ent)))
      )))




;; print database
(defn print-database []
  (println "----------")
  (doseq [dat (vec (map (fn [dat]
                          (let [[e a v tx add] dat]
                            [e a v tx add]))
                        (d/datoms @elite-db-conn :eavt)))]
    (if (some #{(nth dat 1)} '(:galaxy/seed :planet/seed :planet/token-seed))
      (let [[e a v tx add] dat]
        (println [e a
                  (elite/get-seed-bytes v)
                  tx
                  add]))
      (println dat))))


;; check ops for which ones can be executed...

(defn can-perform-operation [prime-op]
  (let [q-map
        (if-let [op-query (get prime-op :query false)]
          (let [op-data-fn (get prime-op :query-data nil)
                 op-data (if (fn? op-data-fn)
                           (op-data-fn)
                           nil)]
            (let [parameter-result (d/q (:query prime-op) @elite-db-conn op-data)
                  applied-result
                  (map (fn [vw]
                         {:op prime-op :data op-data :parameters vw})
                       parameter-result)]
              applied-result))
          {:op prime-op :parameters nil})]
    (if (map? q-map) [q-map] q-map)))

(defn assess-operations [ops]
  (apply concat (map can-perform-operation ops)))

(defn report-problem [description]
  (println (apply str description)))

(defn report-success []
  true)

(defn execute-op! [chosen-op]
  ;;(println chosen-op)
  (if (empty? chosen-op)
    (report-problem "No valid options")
    (let []
      (println "executing op: "(get-in chosen-op [:op :name] nil))
      (let [prime-op (get-in chosen-op [:op] nil)
            exec-fn (get-in prime-op [:exec] nil)
            params (get-in chosen-op [:parameters] nil)
            ]
        (if (or (nil? prime-op) (nil? exec-fn))
          (report-problem ["Op is missing:" chosen-op])
          (let []
            (d/transact!
             elite-db-conn
             (apply exec-fn params))
            true))))))



(defn choose-op
  "Choose an op from the valid subset of the global operations.
  TODO: currently nondetermanistic, make determanistic"
  []
  (let [valid-options (assess-operations operations)]
    (if (empty? valid-options)
      valid-options
      (rand-nth valid-options))))


(comment
  (print-database)
  (assess-operations operations)
  (choose-op)
  (execute-op! (choose-op))
  (print-database))

(def elite-db-conn (d/create-conn elite-schema))
(d/transact! elite-db-conn [{:galaxy/seed (elite/make-seed [0x5A4A 0x0248 0xB753])
                             :galaxy/index 0}
                            {:planet/galaxy 0
                             :planet/index 0
                             :planet/seed (elite/make-seed [0x5A4A 0x0248 0xB753])}
                            ])



(while
    (execute-op! (choose-op)))

(print-database)
(assess-operations operations)
                                        ;operations
(execute-op! (choose-op))
(println operations)
(choose-op)
(export-galaxy)
;;(reset-database!)
