(ns ijk.generator)


;; (defn test-fn [& rest]
;;   ;;(println rest)
;;   [rest])

;; (defn exec-example [input]
;;   (let [id (first input)
;;         in-data (rest input)
;;         out-data (apply test-fn in-data)]
    
;;     [(merge
;;       {:db/id id}
;;       (zipmap [:data/test] out-data))]))

 
;; (exec-example [0 "etst"])




;; (defmacro clause-output-nonexistant [attr]
;;   `[
;;     '(~'not-join [~'?entity]
;;                [~'?entity ~attr ~'_])])

;; (defn clause-out [attr]
;;   `[(~'not-join [~'?entity]
;;      [~'?entity ~attr ~'_])])

;; (vec (apply concat
;;             (map clause-out [:one :two])))

;; (clause-out :test)

;; (vec
;;  (apply concat
;;         (map #(clause-output-nonexistant %) [:test :two])))


;; (map #(clause-output-nonexistant %) [:test :two])
;; (clause-output-nonexistant :test)

;; (macroexpand (clause-output-nonexistant :test))


;; (defmacro not-attribute )

;; (defmacro gen-query [input-keys output-keys]
;;   '[:find ~input-keys
;;     :in $ %
;;     :where
;;     [?unfilled-entity () ?planet-seed]
;;     [?unfilled-planet :planet/government-type ?planet-government]      
;;     (not-join [?unfilled-planet]
;;               [?unfilled-planet :planet/tech-level _] )
;;       ])



;; (defn test-generate-attribute
;;   [{:keys [name input output exec-fn] :as op-data}]
  
;;   {:name name
;;    :input input
;;    :exec
;;    (fn [exec-input]
;;      [(merge {:db/id (first exec-input)}
;;              (apply zipmap output (apply exec-fn (rest exec-input))))
;;       ])
;;    :query
;;    '[]

;;    }
   
;;   )

;; ((get-in
;;   (test-generate-attribute
;;    {:name "planet-tech-level"
;;     :exec-fn test-fn
;;     :input [:planet-seed :planet-economy :planet-government]
;;     :output [:planet-tech-level :test :test]
;;     })
;;   [0 :exec])
;;   [0 "two" "three"])
 


;; (defmacro generate-attribute
;;   [{:keys [name input output exec-fn] :as op-data}]
;;   `{:name ~name
;;    :input ~input
;;    :output ~output
;;    :exec
;;    (fn [exec-input] 
;;      [{:db-id (first exec-input)}
;;       (zipmap ~output (~exec-fn (rest exec-input)))])
;;    :query
;;    '[]
;;    })

;; (generate-attribute
;;  {:name "planet-tech-level"
;;   :exec-fn test-fn
;;   :input [:planet-seed :planet-economy :planet-government]
;;   :output [:planet-tech-level]
;;   })

;; (macroexpand
;;  '(generate-attribute
;;    {:name "planet-tech-level"
;;     :exec-fn test-fn
;;   :input [:planet/index :planet/seed :planet/economy :planet-government]
;;   :output [:planet/tech-level]
;;     }))
;; ;; => {:exec
;; ((fn
;;    [ exec-input]
;;    [{:db-id (clojure.core/first exec-input)}
;;     (clojure.core/zipmap
;;      [:planet/tech-level]
;;      (apply test-fn (clojure.core/rest exec-input)))])
;; ["test2" "data"]

;;  )

;; ;;     :name "planet-tech-level",
;; ;;     :output [:planet/tech-level],
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :query '[]}




;; ;; => {:exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      [{:db-id (clojure.core/first ijk.generator/exec-input)}
;; ;;       (clojure.core/zipmap
;; ;;        [:planet/tech-level]
;; ;;        (test-fn (clojure.core/rest ijk.generator/exec-input)))]),
;; ;;     :name "planet-tech-level",
;; ;;     :output [:planet/tech-level],
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :query '[]}



;; ;; => {:exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      [{:db-id (clojure.core/first ijk.generator/exec-input)}
;; ;;       (clojure.core/zipmap
;; ;;        [:planet/tech-level]
;; ;;        (test-fn (clojure.core/rest ijk.generator/exec-input)))]),
;; ;;     :name "planet-tech-level",
;; ;;     :output [:planet/tech-level],
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :query '[]}

;; ((fn
;;    [exec-input]
;;    [{:db-id (clojure.core/first exec-input)}
;;     (clojure.core/zipmap
;;      [:planet/tech-level]
;;      [0])
;;     (println exec-input)
;;     (test-fn (clojure.core/rest [0 "etstwo"]))
;;     (test-fn (clojure.core/rest exec-input))])
;;  [0 "test"]
;;  )





;; ;; => {:exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      [#:db{:id (clojure.core/first ijk.generator/exec-input)}
;; ;;       (clojure.core/zipmap
;; ;;        [:planet/tech-level]
;; ;;        (clojure.core/apply
;; ;;         test-fn
;; ;;         (clojure.core/rest ijk.generator/exec-input)))]),
;; ;;     :name "planet-tech-level",
;; ;;     :output [:planet/tech-level],
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :query '[]}





;; ;; => {:exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      (clojure.core/let
;; ;;       [ijk.generator/id
;; ;;        (clojure.core/first ijk.generator/exec-input)
;; ;;        ijk.generator/in-data
;; ;;        (clojure.core/rest ijk.generator/exec-input)
;; ;;        ijk.generator/out-data
;; ;;        (clojure.core/apply test-fn ijk.generator/in-data)]
;; ;;       [#:db{:id ijk.generator/id}
;; ;;        (clojure.core/zipmap
;; ;;         [:planet/tech-level]
;; ;;         ijk.generator/out-data)])),
;; ;;     :name "planet-tech-level",
;; ;;     :output [:planet/tech-level],
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :query '[]}








;; ;; => {:name nil,
;; ;;     :input nil,
;; ;;     :output nil,
;; ;;     :exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      (clojure.core/let
;; ;;       [ijk.generator/id
;; ;;        (clojure.core/first ijk.generator/exec-input)
;; ;;        ijk.generator/in-data
;; ;;        (clojure.core/rest ijk.generator/exec-input)
;; ;;        ijk.generator/out-data
;; ;;        (clojure.core/apply nil ijk.generator/in-data)]
;; ;;       [#:db{:id ijk.generator/id}
;; ;;        (clojure.core/zipmap nil ijk.generator/out-data)])),
;; ;;     :query []}



;; ;; => {:name "planet-tech-level",
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :output [:planet/tech-level],
;; ;;     :exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      (clojure.core/let
;; ;;       [{:keys (:planet/seed :planet/economy :planet-government)}
;; ;;        ijk.generator/exec-input]
;; ;;       #:db{:id (clojure.core/first ijk.generator/exec-input)})),
;; ;;     :query []}


;; ;; => {:name "planet-tech-level",
;; ;;     :input
;; ;;     [:planet/index :planet/seed :planet/economy :planet-government],
;; ;;     :output [:planet/tech-level],
;; ;;     :exec
;; ;;     (clojure.core/fn
;; ;;      [ijk.generator/exec-input]
;; ;;      (clojure.core/let
;; ;;       [{:keys
;; ;;         [:planet/index :planet/seed :planet/economy :planet-government]}
;; ;;        ijk.generator/exec-input]
;; ;;       nil)),
;; ;;     :query []}











;; ;; {:exec
;; ;; (fn [p-index p-seed p-econ p-gov]
;; ;;       {:db/id p-index
;; ;;        :planet/tech-level (elite/planet-tech-level p-seed p-econ p-gov)})
;; ;;     :query
;; ;;     '[:find ?unfilled-planet ?planet-seed ?planet-economy ?planet-government
;; ;;       :in $ %
;; ;;       :where
;; ;;       [?unfilled-planet :planet/seed ?planet-seed]
;; ;;       [?unfilled-planet :planet/government-type ?planet-government]      
;; ;;       (not-join [?unfilled-planet]
;; ;;                 [?unfilled-planet :planet/tech-level _] )]}
      

      
