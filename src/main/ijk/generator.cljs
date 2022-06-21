(ns ijk.generator
  (:require-macros [ijk.generator])
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


;;(make-q-sym :test)

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

;; (make-query [:in-one :in-two] [:out-one :out-two])
;;(make-query [:planet/seed] [:planet/size])

 
;; (d/q
;;  '[:find ?entity ?attr-1
;;    :in $
;;    :where
;;    [?entity :planet/seed ?attr-1]
;;    (not-join [?entity] [?entity :planet/size _])
;;    ]
;;  @elite-db-conn)


;; (d/q
;;  (make-query [:planet/seed] [:planet/size])
;;  @elite-db-conn nil)



(defn generate-attribute
  [{:keys [name input output exec-fn] :as op-data}]
  {:name name
   :input input
   :exec
   (fn [& exec-input]
     (let [result (apply exec-fn (rest exec-input))
           ;; Note that if the exec-fn returns anything other than a sequence,
           ;; we need to wrap it in a vector so that it can match up with the
           ;; vector of labels for the attributes.
           result-vec (if (seq? result) result [result])]
       [(merge {:db/id (first exec-input)}
                 (zipmap output result-vec))]))
   :query
   (make-query input output)})

;; (generate-attribute
;;  {:name "planet-tech-level"
;;   :input [:planet/seed :planet/economy-prosperity :planet/government-type]
;;   :output [:planet/tech-level]
;;   :exec-fn elite/planet-tech-level-from-prosperity})


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

;;(choose-op)
;;(execute-op! (choose-op))
;;(print-database)

;; (d/q
;;  '[:find ?planet-index ?allowed-galaxy ;; ?galaxy-count
;;    ;;:with ?planet-index
;;    :in $ [?allowed-galaxy ...]
;;    :where
;;    [?galaxy-id :galaxy/seed ?galaxy-seed]
;;    [?galaxy-id :galaxy/index ?galaxy-index]
;;    [(= ?galaxy-index ?allowed-galaxy)]
;;    [?planet-id :planet/galaxy ?galaxy-index]
;;    [?planet-id :planet/index ?planet-index]
;;    ;;[(ijk.generator/galaxy-planet-count ?galaxy-index) ?galaxy-count]
      
;;     ;;[(max ?planet-index) ?max-planet-index]
;;     ;;[(< ?max-planet-index 4)]
    
;;    ]
;;  @elite-db-conn
;;  [1 2])

;; (galaxy-planet-count 0)


;; (comment 
;; (swap! current-database assoc-in [:db-conn]
;;        elite-db-conn)
;; (swap! current-database assoc-in [:db-schema]
;;        elite-schema))

(def limit-to-galaxy-planet-count 4 ;;256
  )
(def limit-to-galaxy-count 8)

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
   [?planet-id :planet/index ?planet-index]]
 @elite-db-conn galaxy-index))

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
                [?unnamed-planet :planet/name _] )]
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
    }
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
    (if (some #{(nth dat 1)} '(:galaxy/seed :planet/seed :planet/token-seed))
      (let [[e a v tx add] dat]
        (println [e a
                  (elite/get-seed-bytes v)
                  tx
                  add]))
      (println dat))))


;; check ops for which ones can be executed...

(defn can-perform-operation [prime-op]
  ;;(println "\t\t" prime-op)
  (let [q-map
        ;;(let [prime-op (nth operations 0)])
        (if-let [op-query (get prime-op :query false)]
          (let [op-data-fn (get prime-op :query-data nil)
                 op-data (if (fn? op-data-fn)
                           (op-data-fn)
                           nil)]
            (let [parameter-result (d/q (:query prime-op) @elite-db-conn op-data)
                  ;; _ (println "+++ " parameter-result " +++")
                  ;;_ (js/console.log parameter-result)
                  applied-result (map (fn [vw]
                     ;;(println "op:" (str prime-op) "\nV:" vw "\n")
                     {:op prime-op :data op-data :parameters vw}) 
                                      parameter-result)
                  ]
              ;; (println "=> " applied-result "\n")
              applied-result
              ))
          {:op prime-op :parameters nil})]
    (if (map? q-map) [q-map] q-map)))

;;(apply concat (map can-perform-operation operations))
(assess-operations operations)

(defn assess-operations [ops]
  (apply concat (map can-perform-operation ops)))

(defn report-problem [description]
  (println (apply str description)))


(defn execute-op! [chosen-op]
  (if (empty? chosen-op)
    (report-problem "No valid options")
    (let []
      (println (get-in chosen-op [:op :name] nil)
               ;;(apply str (get-in chosen-op [:parameters]))
               )
      (let [prime-op (get-in chosen-op [:op] nil)
            exec-fn (get-in prime-op [:exec] nil)
            params (get-in chosen-op [:parameters] nil)
            ]
        ;; (println "params:" params)
        ;; (println (count params)
        ;;          (count (get prime-op :input []))
        ;;          (get prime-op :input [])
        ;;          )
        (if (or (nil? prime-op) (nil? exec-fn))
          (report-problem ["Op is missing:" chosen-op])
          (d/transact!
           elite-db-conn
           (let [;;_ (println "transact")
                 ;;parameters (into [elite-db-conn] (get-in chosen-op [:parameters] nil))
                 ;;params-vec params
                 ;; _ (println "params is vec? " (vector? params-vec))
                 ;; _ (println (get prime-op :input nil))
                 ;; _ (println params-vec)
                 result (apply
                         exec-fn
                         params)
                         ]
             ;;(println (get-in chosen-op [:parameters]))
             ;;(println result)
             ;;(println "+#+#+#+#+")
             result)))))))


;; (d/transact! elite-db-conn
;;              [{:db/id -1
;;                :test 2}]
;;              )

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
       


(choose-op);; => {:op {:name "planet-tech-level", :input [:planet/seed :planet/economy-prosperity :planet/government-type], :exec #object[Function], :query [:find ?entity ?attr-1 ?attr-2 ?attr-3 :in $ % :where [?entity :planet/seed ?attr-1] [?entity :planet/economy-prosperity ?attr-2] [?entity :planet/government-type ?attr-3] (not-join [?entity] [?entity :planet/tech-level _])]}, :data nil, :parameters [3 #object[DataView [object DataView]] 5 7]}
