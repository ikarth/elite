(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   [ijk.elite-utility :as utility]
   [datascript.core :as d]
   [clojure.spec.alpha :as spec]
   ;;[clojure.edn :as edn]
   [clojure.string :as cstring]
   [clojure.math]
   [cljs.pprint :as pprint]
   ;;[grotesque.core :as grot]
   ;;["js-xxhash" :as xx :refer (xxHash32)]

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log-db
  "Log a complete listing of the entities in the provided `db` to the console."
  [db]
  (d/q '[:find ?any ?obj :where [?obj :type ?any]] @db)) ;todo: log to console...






;; (def database-records
;;   {:elite {:db-conn }})

(defonce current-database
  (atom {:db-conn nil
         :db-schema {}
         }))


(defn setup [])

;; (defn update-database [database-id]
;;   (let [new-database (get database-records database-id :default-database)]
;;     (if (and new-database (get new-database :db-conn false))
;;       (let [swap-in! (fn [db-id]
;;                        (swap! current-database assoc-in [db-id]
;;                               (get new-database db-id)))]
;;         (swap-in! :db-conn)
;;         (swap-in! :db-schema)
;;         )
;;       (println (str "Database not found: " database-id)))))

;; (defn reset-the-database!
;;   [database-id]
;;   (update-database database-id)
;;   (if-let [db-conn (get @current-database :db-conn)]
;;     (let [db-schema (get @current-database :db-conn)]
;;       (d/reset-conn! db-conn (d/empty-db db-schema))
;;       ;; TODO: any initial transactions go here...
;;       db-conn
;;       )))

(comment
  (defn fetch-internal-view
    "Returns the internal database. Intended mostly for debugging visualization."
    []
    (if-let [db-conn (get @current-database :db-conn)]
      (vec (map (fn [dat]
                  (let [[e a v tx add] dat]
                    [e a v tx add])) (d/datoms @db-conn :eavt)))
      (println "Database connection missing when trying to fetch a new view."))))

;; (defn make-empty-project [database-id]
;;   (println (str "Switching to: " database-id))
;;   (assert (not (undefined? database-id)) "Tried to switch to an undefined generator database.")
;;   (update-database database-id)
;;   (println "Resetting database...")
;;   (reset-the-database! database-id))

(spec/def :seed/specifier
  (spec/and
   (spec/coll-of number? :kind vector? :count 3 :into [])
   ;(spec/every #(>= 0 % 255) :count 6)
   ))


(def elite-schema {:seed/planet            {:db/cardinality :db.cardinality/one   :db/unique :db.unique/identity}
                   :seed/description       {:db/cardinality :db.cardinality/one}
                   :seed/galaxy            {:db/cardinality :db.cardinality/one}
                   :planet/economy-type    {:db/cardinality :db.cardinality/one}
                   :planet/species         {:db/cardinality :db.cardinality/one}
                   :planet/government-type {:db/cardinality :db.cardinality/one}
                   :planet/name-length     {:db/cardinality :db.cardinality/one}
                   :planet/partial-name    {:db/cardinality :db.cardinality/one}
                   :planet/name            {:db/cardinality :db.cardinality/one}
                   :planet/description     {:db/cardinality :db.cardinality/one}})







;; (if-let [db-conn (get @current-database :db-conn)]
;;     (vec (map (fn [dat]
;;                 (let [[e a v tx add] dat]
;;                   [e a v tx add])) (d/datoms @db-conn :eavt)))
;;     (println "Database connection missing when trying to fetch a new view."))

;; (update-database :elite)
;; (reset-the-database! :elite)

;; (reset-the-database! :elite
;;                      )

;; (if-let [db-conn (get @current-database :db-conn)]
;;   (d/transact! db-conn [{:test/name "Name of Test"}])
;;   )

;;(fetch-internal-view)

;; (log-db @current-database)






;; .QQ16 in the original code.
;; I'm storing them from 0-31 here instead of the original 128 to 159, because I don't need to cram it into the same memory as the other text glyphs and commands so we save an extra step.
(def elite-planet-name-digraphs
  [
 ""                ;; for planet names, 0 is a special case. I'm encoding this in this table instead of in the function.   ;;"AL"              ; Token 128
 "LE"              ; Token 129
 "XE"              ; Token 130
 "GE"              ; Token 131
 "ZA"              ; Token 132
 "CE"              ; Token 133
 "BI"              ; Token 134
 "SO"              ; Token 135
 "US"              ; Token 136
 "ES"              ; Token 137
 "AR"              ; Token 138
 "MA"              ; Token 139
 "IN"              ; Token 140
 "DI"              ; Token 141
 "RE"              ; Token 142
 "A"               ; Token 143, originally "A?"
 "ER"              ; Token 144
 "AT"              ; Token 145
 "EN"              ; Token 146
 "BE"              ; Token 147
 "RA"              ; Token 148
 "LA"              ; Token 149
 "VE"              ; Token 150
 "TI"              ; Token 151
 "ED"              ; Token 152
 "OR"              ; Token 153
 "QU"              ; Token 154
 "AN"              ; Token 155
 "TE"              ; Token 156
 "IS"              ; Token 157
 "RI"              ; Token 158
 "ON"              ; Token 159
   ]
  )



(defn extract-seed-for-name-length
  "Given a planet seed, determine the length of the name (3 or 4 digraphs)"
  [seed]
  (if (= 1 (utility/get-value-from-seed seed (:s0_lo utility/elite-index) 1 1))
    4
    3))




(defn generate-name-start [seed]
  (let [token-seed seed
        name-length-remaining (extract-seed-for-name-length seed)
        planet-name ""
        ]
    [token-seed name-length-remaining planet-name]))

(defn op-generate-name-start [seed]
  (zipmap [:seed/token-seed
           :number/name-length-remaining
           :string/planet-name-partial]
          (generate-name-start seed)))

;; Ways we could design this DSL:
;;
;; * We could have the op functions explicitly map the outputs to datalog keys.
;;      - Needs to specify for every function
;;      - Most functions just need a straightforward list, so might implement something to auto-assemble a wrapper function.
;; * For inputs, we could consume everything (removing it from the blackboard) and only keep the ones that get output again
;;      - Seems a bit wasteful
;;      - have to remember to specify all of the non-consumed terms all of the time
;;      + relatively straightforward
;; * We could add some kind of indicator to flag stuff that gets consumed
;;      - Seems like it complicates the grammar a lot
;; * We could indicate stuff getting consumed as part of the output
;; * We could ignore the problem for the moment.



(defn generate-name
  "Generate the planet name recursively, using the elite-planet-name-diagraphs table as the source data.
  
  Having this operation be recursive follows the original BBC Elite, plus it lets us demonstrate how the generator can handle partially-constructed artifacts."
  [input]
  (if (string? input)
    input
    (let [[seed-token name-length-remaining name-in-progress] input]
      (if (< name-length-remaining 1)
        name-in-progress
        (let [new-token (utility/twist-seed seed-token)
              index-bits (utility/get-seed-bits seed-token
                                        (:s2_hi utility/elite-index) 3 5)
              digraph-index (utility/bin-to-byte index-bits)
              digraph (get elite-planet-name-digraphs digraph-index)]
          [new-token
           (dec name-length-remaining)
           (str name-in-progress digraph)
           ]
          )))))

 

(defn galactic-coordinates [seed]
  (let [galactic-x (utility/bin-to-byte (utility/get-seed-bits seed (:s1_hi utility/elite-index) 0 7))
        galactic-y (/ (utility/bin-to-byte (utility/get-seed-bits seed (:s0_hi utility/elite-index) 0 7)) 2)
        size-on-map (utility/bin-to-byte (mapv max [0 1 0 1 0 0 0 0] (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 7)))
        ]
    {:planet/galactic-x  galactic-x
     :planet/galactic-y  galactic-y
     :planet/size-on-map size-on-map}))


(defn galactic-x [seed]
  (utility/bin-to-byte (utility/get-seed-bits seed (:s1_hi utility/elite-index) 0 8)))

(defn galactic-y [seed]
  (utility/bin-to-byte (utility/get-seed-bits seed (:s0_hi utility/elite-index) 0 8)))

(defn size-on-map [seed]
  (utility/bin-to-byte (mapv max [0 1 0 1 0 0 0 0] (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 8))))

(defn planet-government
  "Planet government is a number from 0 to 7, extracted directly from the bits in the seed.

  The first operation in the original code, and the most basic."  
  [planet-seed]  
  (utility/bin-to-byte
   (utility/get-seed-bits planet-seed 
                  (:s1_lo utility/elite-index)
                  2 3)))



(defn planet-economy
  "Generate the economic level and type of the planet.
  Note that we need the governement type, because anarchy and feudal governements are restricted from being rich.
  We *could* just calculate the government type on the fly, since in the original algorithm this was all one dense code block that fed into the next step, but for the purposes of our more flexible generator, I'm opting to make it a parameter - maybe there's some new operation that alters the government type or something, so we'll want the final value instead of a parallel calculation. But in other circumstances we might opt for the parallel calculation."
  [planet-seed planet-government]
  (let [eco-base
        (try 
          (utility/get-seed-bits planet-seed
                         (:s0_hi utility/elite-index)
                         5 3)
          (catch js/Error e
            (println "An economic error occurred:" e)
              0))
        adjusted (assoc eco-base 1 (if (< planet-government 2) 1 (nth eco-base 1)))
        type (nth eco-base 0)
        prosperity (try
                     (utility/bin-to-byte adjusted)
                     (catch js/Error e
                       (println "An economic error occurred: " e)
                       0
                       ))
        ;; Returning both the prosperity-coerced-to-an-integer *and* the flipped number
        ;; is an example of how the original code compactly reuses its computational
        ;; resources, in contrast with how our more-modular reconstruction needs to
        ;; separate the values. I could avoid this if I just kept it as the bytes
        ;; (or if I had a byte-flipping function)
        ;;flipped-economy (bin-to-byte (invert-bits adjusted))
        ;; ...on second thought, I wrote the byte-flipping function.
        ]
    ;;(println [eco-base adjusted prosperity])
    [type prosperity]))


(defn planet-tech-level
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)" 
  [planet-seed economy government]
  (+ (utility/invert-byte (second economy) 3)
     (utility/bin-to-byte
      (utility/get-seed-bits planet-seed (:s1_hi utility/elite-index) 6 2))
     (clojure.math/ceil (/ government 2))))


(defn planet-tech-level-from-prosperity
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)

  There are two of these functions because the ecological query was
  easier to write if it used prosperity directly." 
  [planet-seed prosperity government]
  (+ (utility/invert-byte prosperity 3)
     (utility/bin-to-byte
      (utility/get-seed-bits planet-seed
                     (:s1_hi utility/elite-index)
                     6
                     2))
     (clojure.math/ceil (/ government 2))))


;; 2022/9/6 - There is a bug in the species generation. For example, 252 Tiinlebi is
;;            reporting "Green Horned Felines" when it should be "Green Horned Humanoids"
;; 2022/9/6 - The bug was in this table: "Lobster" was missing from the list.

(def species-table
  [["Large ", "Fierce ", "Small ", "", "", "", "", ""]
  ["Green " "Red " "Yellow" "Blue " "Black " "Harmless " "" "" "" ""]
  ["Slimy " "Bug-eyed " "Horned " "Bony " "Fat ", "Furry ", "", ""]
  ["Rodents"
   "Frogs"
   "Lizards"
   "Lobsters"
   "Birds"
   "Humanoids"
   "Felines"
   "Insects"]
   ])



(defn planet-goat-soup
  "Generate the 'goat soup' planet description string"
  [seed system-name]
  "goat soup"
  (egrammar/generate-goat-soup seed system-name)
  )

(defn planet-species
  "Generate a (brief) description of the local planetary species."
  [seed]
   ;; Check it 7 of s2_lo - if it is zero return "Human Colonials"
  (if (= 0 
         (first (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 1))
         )
    "Human Colonials"
    (let [;; A bunch of fiddly bitmapping operations. Remember that our subvec indexing is counting from the opposite direction as the notes on the BBC Elite site...
          ;; register-A = s2_hi
          register-A 
          (vec (concat [0 0]
                       (into [] (utility/get-seed-bits seed (:s2_hi utility/elite-index) 0 6))))
          ;; bits 2-4 of A, by using a mask which multiplies the bit strings together 
          species-size  (utility/bin-to-byte (mapv * [0 0 0 0 0 1 1 1] register-A))
          ;; bits 5-7 of A, by just grabbing the subvec 
          species-color (subvec register-A 3 5)
          ;; A = bits 0-2 of (s0_hi EOR s1_hi)
          register-A-new
          (mapv (fn [a b] (if (not= a b) 1 0))
                (utility/get-seed-bits seed (:s0_hi utility/elite-index) 0 8)
                (utility/get-seed-bits seed (:s1_hi utility/elite-index) 0 8))
          ;; texture = bits 0-2 of the new A
          texture (mapv * [0 0 0 0 0 1 1 1]
                        register-A-new)
          ;; bits 0-1 of s2_hi
          intermediate-B (mapv * [0 0 0 0 0 0 1 1]
                           (utility/get-seed-bits seed (:s2_hi utility/elite-index) 0 8))
          ;; add register B to A-new
          intermediate-B-plus (utility/bitwise-add-vec texture
                                           (mapv * [0 0 0 0 0 0 1 1]
                                                 intermediate-B))
          ;; take bits 0-2 of B-plus
          species-name (mapv * [0 0 0 0 0 1 1 1]
                             intermediate-B-plus)         
          species-type (utility/bin-to-byte (utility/get-seed-bits seed (:s2_hi utility/elite-index) 6 2))
          species-id  [(utility/bin-to-byte (utility/get-seed-bits seed (:s2_hi utility/elite-index) 3 3)) ;; size
                       (utility/bin-to-byte (utility/get-seed-bits seed (:s2_hi utility/elite-index) 0 3)) ;; color
                       (utility/bin-to-byte (subvec texture 5)) ;; texture                  
                       (utility/bin-to-byte species-name)]] ;; type
      (apply str 
       (map #(get %2 %1) species-id species-table)))))



;; Test generating species descriptions and planet names...
(comment
  (planet-species (utility/make-seed [0x57fa 0x1d30 0x17b3]))
  (planet-species (utility/make-seed [0x588a 0x476c 0x02db]))
  (last (take 6
              (iterate
               generate-name (generate-name-start (utility/make-seed [0xfa57 0x301d 0xb317])))))
  (last (take 6
              (iterate
               generate-name (generate-name-start (utility/make-seed [0x57fa 0x1d30 0x17b3]))))))


(defn planet-population-size [tech-level economy government]
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (or (vector? economy)
                (seq? economy))
          (second economy)
          economy)]
  (+
   (* tech-level 4)
   econ-prosperity
   government
   1)))

(seq? [0 1])
(second (seq [0 1]))
(planet-population-size 4 [0 0] 2)

(defn planet-productivity [economy government population]
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (or (vector? economy)(seq? economy))
          (second economy)
          economy)]
  (* 8
     (+ (utility/invert-byte econ-prosperity 3) 3)
     (+ government 4)
     population
     )))

(defn government-name [gov-type]
  (nth [:anarchy :feudal :multi-government :dictatorship :communist :confederacy :democracy :corporate-state]
       gov-type
       :unknown))

(defn economy-name [[type prosperity]]
  [
   (get {0 :rich
         1 :average
         2 :poor
         3 :mainly
         4 :mainly
         5 :rich
         6 :average
         7 :poor}
        prosperity)
   (nth [:industrial :agricultural] type)])


(defn list-reachable-systems
  "Given an indexed list of planet coordinates, return a list of planets within jump range."
  [system-coordinates list-of-planet-coordinates jump-range]
  (filterv (fn [p] (and (< 0 (second p)) (<= (second p) jump-range)))
           (mapv (fn [p] [(first p) (utility/distance-2d-bbc-elite system-coordinates (second p))]) list-of-planet-coordinates)))


(defn calculate-hub-count
  "Given a list of x,y galactic coordinates, figure out how many systems are in jump range of this system"
  [system-coordinates list-of-planet-coordinates jump-range]
  (count (list-reachable-systems system-coordinates list-of-planet-coordinates jump-range)
   ))


;; The seed for the orignal game, chosen after searching through many possibilities...
(def elite-seed (utility/make-seed [0x5A4A 0x0248 0xB753]))

;; test twisting the seed
(def planet-two (-> elite-seed
                    utility/twist-seed
                    utility/twist-seed
                    utility/twist-seed
                    utility/twist-seed))

;; test getting all of the planets
(def planet-seed-list
  (reduce (fn [current next-id] (concat current [(utility/twist-to-next-planet (last current))]))
          [elite-seed]
          (range 255)))


(def auth-tech [9 7 8 12 7 10 9 5 12 7 3 10 9 7 7 8 9 8 7 7 9 4 11 14 8])

;; test starting the generation of a name
(let [[a b c] (generate-name-start elite-seed)]
  (generate-name [a b c]))

;; test recursively generating a name
(generate-name
 (generate-name
  (generate-name
   (generate-name
    (generate-name
     (generate-name-start elite-seed))))))

(last (take 7 (iterate generate-name (generate-name-start planet-two))))


(let [e1 (last (take 7 (iterate generate-name (generate-name-start elite-seed))))
      e2 (last (take 7 (iterate generate-name (generate-name-start planet-two))))]
  [(= e1 "TIBEDIED")
   e1
   (= e2 "QUBE")
   e2
   ])

(let []
  (doseq [[r p] (map-indexed (fn [index item] [index item]) planet-seed-list)]
    (let [name (last (take 6 (iterate generate-name (generate-name-start p))))
          ]
      (println (str r "\t" name)))))



(defn test-galaxy-locations []
  (mapv (fn [p] [(galactic-x p) (galactic-y p)]) planet-seed-list))

(test-galaxy-locations)
(mapv #(utility/distance-2d [0 0] %) (test-galaxy-locations))
(utility/distance-2d [0 0] [5 7])

(js/Math.sqrt 4)

(defn round-pop [pop]
  (/ (.round js/Math (* 10 pop)) 10))

(defn test-galaxy-generator []
  (let []
    (println "\n\n\n\n\n\n\n\n\n\n\n\n")
    (doseq [[r p] (map-indexed (fn [index item]
                                 [index item])
                               planet-seed-list)
           
            ]
      
      (let [planet-coord-list (map-indexed (fn [index p] [index [(galactic-x p) (galactic-y p)]]) planet-seed-list)
            jump-range 70
            gov (planet-government p)
            econ (planet-economy p (planet-government p))
            tech (planet-tech-level p econ gov)
            pop (planet-population-size tech econ gov)
            prod (planet-productivity econ gov pop)
            name (last (take 7 (iterate generate-name (generate-name-start p))))
            species (planet-species p)
            galactic-x (galactic-x p)
            galactic-y (galactic-y p)
            reachable-systems (list-reachable-systems [galactic-x galactic-y] planet-coord-list jump-range)
            ;;hub-count (calculate-hub-count [galactic-x galactic-y] planet-coord-list jump-range)
            hub-count (count reachable-systems)
            goat-soup-string (egrammar/generate-goat-soup p name)
            ]
        (if true;(< 245 r )
          (println 
           (map 
            #(str %1 ":\t "%2 "\n")
            ["id" "name" "seed" "species" "government" "economy" "tech-level" "pop. size" "productivity" "gal. coords" "neighbors" "hub count" "description"]
            [r
             name
             (map #(. % toString 16) (utility/get-seed-bytes p))
             species
             [gov (government-name gov)]
             [econ (economy-name econ)]
             tech
             [pop (str (round-pop (* pop 0.1)) " billion")]
             prod
             [galactic-x galactic-y]
             reachable-systems
             hub-count
             goat-soup-string
             ])))))))



;;(test-galaxy-generator)

(defn run []
  (.log js/console "Hello")
  (println "World"))

(def original-planet-descriptions
  [
   [ 0, "Tibedied", "This planet is most notable for Tibediedian Arnu brandy but ravaged by unpredictable solar activity."],
   [ 1, "Qube", "Qube is reasonably well known for its great dense forests but scourged by deadly civil war."],
   [ 2, "Leleer", "The world Leleer is very noted for its pink Leleerian Itonthbi tulip plantations but plagued by deadly earthquakes."],
   [ 3, "Biarge", "This world is very fabled for the Biargian edible poet."],
   [ 4, "Xequerin", "The world Xequerin is fabled for its weird volcanoes and the Xequerinian mountain lobstoid."],
   [ 5, "Tiraor", "Tiraor is a revolting little planet."],
   [ 6, "Rabedira", "The planet Rabedira is well known for its inhabitants' ancient loathing of sit coms but ravaged by dreadful civil war."],
   [ 7, "Lave", "Lave is most famous for its vast rain forests and the Lavian tree grub."],
   [ 8, "Zaatxe", "This planet is mildly noted for the Zaatxian deadly Atlenooid but ravaged by lethal lethal yaks."],
   [ 9, "Diusreza", "This planet is mildly fabled for its inhabitants' eccentric love for tourists but beset by deadly edible moths."],
   [ 10, "Teaatis", "Teaatis is mildly well known for Teaatisian vicious brew."],
   [ 11, "Riinus", "This world is mildly famous for its vast rain forests and the Riinusian tree grub."],
   [ 12, "Esbiza", "The planet Esbiza is most famous for its vast rain forests."],
   [ 13, "Ontimaxe", "This planet is reasonably famous for its inhabitants' exceptional love for food blenders."],
   [ 14, "Cebetela", "This world is most notable for its fabulous Cebetelian lethal brandy but scourged by killer mountain Esbionoids."],
   [ 15, "Ceedra", "This planet is most fabled for its inhabitants' ingrained silliness but scourged by deadly civil war."],
   [ 16, "Rizala", "The planet Rizala is mildly notable for Rizalian lethal brandy."],
   [ 17, "Atriso", "Atriso is mildly well known for its exotic cuisine and its inhabitants' ingrained silliness."],
   [ 18, "Teanrebi", "This planet is plagued by frequent earthquakes."],
   [ 19, "Azaqu", "The planet Azaqu is most famous for its pink oceans and Zero-G cricket."],
   [ 20, "Retila", "This world is ravaged by occasional solar activity."],
   [ 21, "Sotiqu", "The planet Sotiqu is famous for its exotic goat soup but ravaged by a killer disease."],
   [ 22, "Inleus", "The world Inleus is most famous for the Inleusian spotted wolf."],
   [ 23, "Onrira", "The world Onrira is mildly noted for the Onririan deadly Seoid but cursed by dreadful solar activity."],
   [ 24, "Ceinzala", "This planet is most notable for vicious Numaab juice but cursed by unpredictable solar activity."],
   [ 25, "Biisza", "The planet Biisza is most famous for its vast rain forests."],
   [ 26, "Legees", "This planet is most notable for its exotic night life but ravaged by frequent earthquakes."],
   [ 27, "Quator", "The world Quator is scourged by deadly edible arts graduates."],
   [ 28, "Arexe", "The world Arexe is fabled for its exciting sit coms and its inhabitants' ancient loathing of sit coms."],
   [ 29, "Atrabiin", "Atrabiin is cursed by killer edible Nuatoids."],
   [ 30, "Usanat", "The world Usanat is a boring world."],
   [ 31, "Xeesle", "The world Xeesle is a boring planet."],
   [ 32, "Oreseren", "Oreseren is a revolting little planet."],
   [ 33, "Inera", "This planet is noted for its exotic fish meat."],
   [ 34, "Inus", "This world is reasonably well known for the Inusian tree wolf but scourged by unpredictable earthquakes."],
   [ 35, "Isence", "The world Isence is very famous for its unusual casinos but beset by a evil disease."],
   [ 36, "Reesdice", "The world Reesdice is reasonably famous for the Reesdician deadly lobstoid."],
   [ 37, "Terea", "This world is very fabled for the Terian edible poet."],
   [ 38, "Orgetibe", "This planet is a dull world."],
   [ 39, "Reorte", "This planet is mildly fabled for its inhabitants' eccentric love for tourists but plagued by deadly earthquakes."],
   [ 40, "Ququor", "The planet Ququor is mildly well known for its exotic cuisine."],
   [ 41, "Geinona", "This world is ravaged by unpredictable solar activity."],
   [ 42, "Anarlaqu", "This world is mildly famous for its hoopy night life and its exotic night life."],
   [ 43, "Oresri", "The planet Oresri is cursed by dreadful civil war."],
   [ 44, "Esesla", "This planet is noted for Zero-G hockey."],
   [ 45, "Socelage", "This planet is reasonably noted for its exotic goat meat."],
   [ 46, "Riedquat", "This planet is most notable for its fabulous cuisine but beset by occasional civil war."],
   [ 47, "Gerege", "The world Gerege is reasonably famous for the Geregian spotted wolf."],
   [ 48, "Usle", "This world is very notable for the Uslian tree ant and its inhabitants' exceptional loathing of sit coms."],
   [ 49, "Malama", "The planet Malama is mildly well known for its exotic cuisine."],
   [ 50, "Aesbion", "The planet Aesbion is cursed by dreadful civil war."],
   [ 51, "Alaza", "The world Alaza is scourged by a evil disease."],
   [ 52, "Xeaqu", "The world Xeaqu is a dull place."],
   [ 53, "Raoror", "This world is very fabled for its weird volcanoes."],
   [ 54, "Ororqu", "The planet Ororqu is well known for its inhabitants' ancient mating traditions but ravaged by unpredictable solar activity."],
   [ 55, "Leesti", "The planet Leesti is reasonably fabled for Zero-G cricket and Leestian evil juice."],
   [ 56, "Geisgeza", "This planet is notable for its unusual oceans and the Geisgezian mountain slug."],
   [ 57, "Zainlabi", "This world is ravaged by unpredictable civil war."],
   [ 58, "Uscela", "The world Uscela is a boring world."],
   [ 59, "Isveve", "The planet Isveve is reasonably noted for its inhabitants' eccentric shyness and its inhabitants' eccentric shyness."],
   [ 60, "Tioranin", "This world is most notable for Tioraninian vicious brew but ravaged by unpredictable civil war."],
   [ 61, "Learorce", "Learorce is reasonably notable for its great dense forests but scourged by deadly edible poets."],
   [ 62, "Esusti", "This world is very well known for its inhabitants' ancient loathing of discos and the Esustian spotted cat."],
   [ 63, "Ususor", "This planet is very notable for its inhabitants' weird shyness and the Ususorian edible poet."],
   [ 64, "Maregeis", "This world is fabled for its ancient Maregeisian Onbidi tulip plantations."],
   [ 65, "Aate", "The world Aate is scourged by killer mountain lobstoids."],
   [ 66, "Sori", "The world Sori is beset by a evil disease."],
   [ 67, "Cemave", "The world Cemave is beset by dreadful earthquakes."],
   [ 68, "Arusqudi", "This world is very fabled for its unusual oceans."],
   [ 69, "Eredve", "This planet is beset by a evil disease."],
   [ 70, "Regeatge", "Regeatge is reasonably well known for its great dense forests but scourged by frequent civil war."],
   [ 71, "Edinso", "This planet is mildly noted for its pink Edinsian Ge Bemaarleweed plantations but ravaged by vicious mountain monkeys."],
   [ 72, "Ra", "The world Ra is beset by deadly earthquakes."],
   [ 73, "Aronar", "Aronar is most famous for the Aronarian deadly goat and its hoopy casinos."],
   [ 74, "Arraesso", "This planet is notable for its unusual oceans and its inhabitants' exceptional loathing of food blenders."],
   [ 75, "Cevege", "This world is a revolting dump."],
   [ 76, "Orteve", "This world is fabled for its fabulous vicious Ougeza juice."],
   [ 77, "Geerra", "This planet is reasonably noted for its exotic goat soup."],
   [ 78, "Soinuste", "This planet is beset by deadly earthquakes."],
   [ 79, "Erlage", "This world is reasonably well known for the Erlagian tree ant but cursed by vicious mountain goats."],
   [ 80, "Xeaan", "This world is ravaged by unpredictable civil war."],
   [ 81, "Veis", "The planet Veis is a boring world."],
   [ 82, "Ensoreus", "This planet is a tedious little planet."],
   [ 83, "Riveis", "The world Riveis is most well known for its hoopy casinos."],
   [ 84, "Bivea", "This planet is plagued by frequent solar activity."],
   [ 85, "Ermaso", "This planet is very notable for the Ermasian edible grub and the Ermasian tree ant."],
   [ 86, "Velete", "Velete is a revolting dump."],
   [ 87, "Engema", "The world Engema is beset by a evil disease."],
   [ 88, "Atrienxe", "Atrienxe is an unremarkable dump."],
   [ 89, "Beusrior", "The world Beusrior is a dull world."],
   [ 90, "Ontiat", "The planet Ontiat is scourged by a evil disease."],
   [ 91, "Atarza", "This world is plagued by occasional solar activity."],
   [ 92, "Arazaes", "This planet is very notable for the Arazaesian tree ant and Arazaesian wolf meat."],
   [ 93, "Xeeranre", "Xeeranre is cursed by killer mountain Reetaboids."],
   [ 94, "Quzadi", "Quzadi is cursed by dreadful civil war."],
   [ 95, "Isti", "The planet Isti is reasonably noted for its inhabitants' eccentric shyness and Zero-G hockey."],
   [ 96, "Digebiti", "Digebiti is cursed by killer mountain Seoids."],
   [ 97, "Leoned", "Leoned is reasonably well known for the Leonedian tree snake but scourged by unpredictable earthquakes."],
   [ 98, "Enzaer", "Enzaer is a revolting dump."],
   [ 99, "Teraed", "Teraed is an unremarkable dump."],
   [100, "Vetitice", "This world is very well known for Vetitician lethal brandy and its great parking meters."],
   [101, "Laenin", "The planet Laenin is famous for its inhabitants' ancient loathing of sit coms but cursed by a killer disease."],
   [102, "Beraanxe", "The world Beraanxe is reasonably noted for its inhabitants' exceptional love for tourists and its unusual oceans."],
   [103, "Atage", "Atage is an unremarkable planet."],
   [104, "Veisti", "The planet Veisti is reasonably noted for its inhabitants' eccentric shyness and Zero-G cricket."],
   [105, "Zaerla", "The planet Zaerla is mildly well known for its exotic cuisine."],
   [106, "Esredice", "The world Esredice is a boring planet."],
   [107, "Beor", "Beor is an unremarkable dump."],
   [108, "Orso", "The world Orso is reasonably fabled for its exciting sit coms and its inhabitants' exceptional love for food blenders."],
   [109, "Usatqura", "This planet is reasonably famous for its inhabitants' exceptional loathing of sit coms."],
   [110, "Erbiti", "The world Erbiti is most well known for its great dense forests."],
   [111, "Reinen", "This planet is a tedious little planet."],
   [112, "Ininbi", "The world Ininbi is reasonably famous for its inhabitants' exceptional loathing of casinos."],
   [113, "Erlaza", "The world Erlaza is mildly noted for its ancient mountains but plagued by a lethal disease."],
   [114, "Celabile", "The planet Celabile is most famous for the Celabilian evil poet and Zero-G hockey."],
   [115, "Ribiso", "This planet is fabled for its exciting vacuum cricket."],
   [116, "Qudira", "The world Qudira is reasonably fabled for Qudirian Ouarma gargle blasters and the Qudirian evil talking treeoid."],
   [117, "Isdibi", "The world Isdibi is scourged by deadly tree ants."],
   [118, "Gequre", "This world is reasonably well known for the Gequrian tree ant but ravaged by dreadful civil war."],
   [119, "Rarere", "The planet Rarere is mildly notable for Rarerian lethal brandy."],
   [120, "Aerater", "Aerater is a revolting little planet."],
   [121, "Atbevete", "The planet Atbevete is mildly well known for killer Ou gargle blasters."],
   [122, "Bioris", "Bioris is very fabled for the Biorisian edible poet."],
   [123, "Raale", "This world is very fabled for the Raalian edible poet."],
   [124, "Tionisla", "This world is very notable for its inhabitants' ingrained shyness."],
   [125, "Encereso", "Encereso is cursed by dreadful civil war."],
   [126, "Anerbe", "The world Anerbe is reasonably fabled for its exciting vacuum karate and its great volcanoes."],
   [127, "Gelaed", "The planet Gelaed is very noted for its pink Gelaedian Ines Soweed plantations but ravaged by unpredictable civil war."],
   [128, "Onusorle", "This world is mildly well known for Onusorlian vicious brew and Onusorlian wolf cutlet."],
   [129, "Zaonce", "This planet is a tedious place."],
   [130, "Diquer", "The world Diquer is a dull place."],
   [131, "Zadies", "The planet Zadies is famous for its inhabitants' exceptional love for food blenders but scourged by dreadful solar activity."],
   [132, "Entizadi", "The planet Entizadi is famous for its inhabitants' exceptional love for food blenders but scourged by dreadful solar activity."],
   [133, "Esanbe", "Esanbe is famous for its inhabitants' ancient loathing of casinos but plagued by deadly earthquakes."],
   [134, "Usralaat", "This planet is plagued by deadly earthquakes."],
   [135, "Anlere", "Anlere is reasonably well known for the Anlerian spotted shrew but plagued by evil tree leopards."],
   [136, "Teveri", "The world Teveri is reasonably fabled for Teverian evil juice and its inhabitants' ingrained shyness."],
   [137, "Sotiera", "The world Sotiera is mildly fabled for the Sotierian mountain poet but cursed by unpredictable earthquakes."],
   [138, "Ededleen", "The planet Ededleen is mildly well known for its exotic cuisine."],
   [139, "Inonri", "This world is very well known for Inonrian wolf meat and its weird volcanoes."],
   [140, "Esbeus", "The world Esbeus is mildly noted for its ancient mountains but plagued by frequent earthquakes."],
   [141, "Lerelace", "This planet is a dull place."],
   [142, "Eszaraxe", "The planet Eszaraxe is most famous for the Eszaraxian spotted shrew and the Eszaraxian mountain poet."],
   [143, "Anbeen", "Anbeen is reasonably notable for its great tropical forests but cursed by dreadful solar activity."],
   [144, "Biorle", "The world Biorle is a dull world."],
   [145, "Anisor", "This planet is very well known for its inhabitants' ancient mating traditions and its inhabitants' ancient loathing of casinos."],
   [146, "Usrarema", "This world is very notable for the Usraremian edible poet."],
   [147, "Diso", "This planet is mildly noted for its ancient Ma corn plantations but beset by frequent solar activity."],
   [148, "Riraes", "The world Riraes is fabled for its weird rock formations and its pink oceans."],
   [149, "Orrira", "The planet Orrira is cursed by killer edible talking treeoids."],
   [150, "Xeer", "This world is very well known for Xeerian wolf meat and its fabulous cuisine."],
   [151, "Ceesxe", "The world Ceesxe is most well known for its vast rain forests."],
   [152, "Isatre", "The world Isatre is a boring planet."],
   [153, "Aona", "This world is very well known for Aonian lethal brandy and its great volcanoes."],
   [154, "Isinor", "This world is very fabled for its unusual oceans."],
   [155, "Uszaa", "The planet Uszaa is reasonably noted for its inhabitants' eccentric love for tourists and the Uszaian tree grub."],
   [156, "Aanbiat", "This planet is fabled for its ancient Aanbiatian Noalin banana plantations."],
   [157, "Bemaera", "Bemaera is most noted for the Bemaerian deadly Xesooid and its inhabitants' unusual silliness."],
   [158, "Inines", "This world is a tedious place."],
   [159, "Edzaon", "This world is most notable for Edzaonian lethal water but plagued by occasional solar activity."],
   [160, "Leritean", "The planet Leritean is mildly well known for its exotic cuisine."],
   [161, "Veale", "The world Veale is most well known for its vast dense forests."],
   [162, "Edle", "Edle is famous for its inhabitants' exceptional love for food blenders but scourged by frequent civil war."],
   [163, "Anlama", "This world is a tedious little planet."],
   [164, "Ribilebi", "The planet Ribilebi is most famous for its vast oceans and its fabulous goat soup."],
   [165, "Relaes", "This world is a tedious place."],
   [166, "Dizaoner", "Dizaoner is ravaged by unpredictable solar activity."],
   [167, "Razaar", "The world Razaar is a dull place."],
   [168, "Enonla", "Enonla is ravaged by dreadful civil war."],
   [169, "Isanlequ", "This planet is beset by a evil disease."],
   [170, "Tibecea", "Tibecea is very fabled for the Tibecian edible poet."],
   [171, "Sotera", "Sotera is mildly notable for Soterian lethal brandy."],
   [172, "Esveor", "Esveor is mildly famous for its pink oceans and Zero-G hockey."],
   [173, "Esteonbi", "This planet is mildly fabled for its inhabitants' ingrained shyness but cursed by unpredictable solar activity."],
   [174, "Xeesenri", "Xeesenri is mildly notable for its inhabitants' weird shyness."],
   [175, "Oresle", "This world is reasonably notable for its great volcanoes but ravaged by a vicious disease."],
   [176, "Ervein", "Ervein is a revolting little planet."],
   [177, "Larais", "This world is a revolting dump."],
   [178, "Anxebiza", "The planet Anxebiza is an unremarkable dump."],
   [179, "Diedar", "This world is ravaged by dreadful civil war."],
   [180, "Eninre", "The planet Eninre is cursed by deadly civil war."],
   [181, "Bibe", "This world is most fabled for Bibian lethal brandy but beset by a evil disease."],
   [182, "Diquxe", "This planet is mildly noted for its ancient mountains but plagued by frequent earthquakes."],
   [183, "Sorace", "Sorace is cursed by deadly civil war."],
   [184, "Anxeonis", "The planet Anxeonis is most famous for its vast rain forests."],
   [185, "Riantiat", "This planet is notable for the Riantiatian edible grub and the Riantiatian spotted wolf."],
   [186, "Zarece", "This planet is a tedious place."],
   [187, "Maesin", "The planet Maesin is an unremarkable dump."],
   [188, "Tibionis", "Tibionis is most noted for the Tibionisian deadly goat and its vast rain forests."],
   [189, "Gelegeus", "Gelegeus is mildly notable for Gelegeusian Bidialst brandy."],
   [190, "Diora", "The planet Diora is an unremarkable planet."],
   [191, "Rigeti", "Rigeti is a revolting dump."],
   [192, "Begeabi", "Begeabi is very notable for its inhabitants' weird silliness."],
   [193, "Orrere", "Orrere is mildly well known for Orrerian vicious brew."],
   [194, "Beti", "This planet is fabled for its weird volcanoes and the Betian mountain lobstoid."],
   [195, "Gerete", "This world is most fabled for Zero-G cricket but cursed by unpredictable solar activity."],
   [196, "Qucerere", "This planet is a tedious place."],
   [197, "Xeoner", "The world Xeoner is a dull world."],
   [198, "Xezaor", "The world Xezaor is most well known for its hoopy casinos."],
   [199, "Ritila", "The world Ritila is very famous for its hoopy casinos but beset by a evil disease."],
   [200, "Edorte", "The planet Edorte is an unremarkable dump."],
   [201, "Zaalela", "This world is noted for its fabulous goat soup."],
   [202, "Biisorte", "This world is most notable for its fabulous Biisortian lethal water but beset by a lethal disease."],
   [203, "Beesor", "This world is plagued by deadly earthquakes."],
   [204, "Oresqu", "Oresqu is mildly notable for its inhabitants' unusual mating traditions."],
   [205, "Xeququti", "This planet is beset by dreadful earthquakes."],
   [206, "Maises", "Maises is reasonably notable for its fabulous Maisesian lethal water but beset by a lethal disease."],
   [207, "Bierle", "The planet Bierle is most famous for the Bierlian deadly Inoid and its inhabitants' ingrained silliness."],
   [208, "Arzaso", "Arzaso is an unremarkable planet."],
   [209, "Teen", "Teen is cursed by deadly civil war."],
   [210, "Riredi", "This world is very fabled for the Riredian mountain slug."],
   [211, "Teorge", "This planet is a tedious little planet."],
   [212, "Vebege", "The world Vebege is mildly fabled for the Vebegian mountain lobstoid but beset by deadly solar activity."],
   [213, "Xeenle", "This planet is mildly noted for its ancient mountains but plagued by a lethal disease."],
   [214, "Arxeza", "The world Arxeza is beset by dreadful earthquakes."],
   [215, "Edreor", "The world Edreor is reasonably fabled for its fabulous killer Sese juice and its ancient Edreorian Esnu corn plantations."],
   [216, "Esgerean", "This planet is plagued by occasional solar activity."],
   [217, "Ditiza", "The planet Ditiza is reasonably fabled for Ditizian evil juice and its inhabitants' ingrained silliness."],
   [218, "Anle", "The world Anle is notable for its great tropical forests and Anlian evil brandy."],
   [219, "Onisqu", "This planet is a dull place."],
   [220, "Aleusqu", "This world is reasonably notable for its great volcanoes but ravaged by vicious vicious shrews."],
   [221, "Zasoceat", "Zasoceat is a revolting dump."],
   [222, "Rilace", "The world Rilace is a dull world."],
   [223, "Beenri", "This planet is mildly noted for the Beenrian mountain Esseina?oid but scourged by frequent civil war."],
   [224, "Laeden", "The planet Laeden is reasonably fabled for its exciting sit coms and its inhabitants' exceptional love for food blenders."],
   [225, "Mariar", "This world is fabled for its unusual tropical forests."],
   [226, "Riiser", "Riiser is cursed by dreadful civil war."],
   [227, "Qutiri", "The world Qutiri is mildly noted for its ancient mountains but plagued by deadly earthquakes."],
   [228, "Biramabi", "The world Biramabi is a dull world."],
   [229, "Soorbi", "The planet Soorbi is an unremarkable dump."],
   [230, "Solageon", "This world is very well known for Solageonian lethal water and the Solageonian tree wolf."],
   [231, "Tiquat", "This world is reasonably well known for its great parking meters but cursed by unpredictable earthquakes."],
   [232, "Rexebe", "This world is mildly famous for its hoopy night life and its exotic cuisine."],
   [233, "Qubeen", "This world is ravaged by unpredictable civil war."],
   [234, "Cetiisqu", "This planet is reasonably famous for the Cetiisqian evil Stoid."],
   [235, "Rebia", "Rebia is very notable for its inhabitants' weird shyness."],
   [236, "Ordima", "This planet is reasonably noted for its exotic goat soup."],
   [237, "Aruszati", "This planet is noted for Zero-G cricket."],
   [238, "Zaleriza", "This world is a tedious place."],
   [239, "Zasoer", "Zasoer is mildly well known for its exotic night life."],
   [240, "Raleen", "This planet is notable for the Raleenian tree grub and its inhabitants' unusual silliness."],
   [241, "Qurave", "The planet Qurave is mildly notable for Quravian Zaaronen brandy."],
   [242, "Atrebibi", "The world Atrebibi is most famous for the Atrebibian deadly monkey."],
   [243, "Teesdi", "Teesdi is famous for Teesdian shrew cutlet but ravaged by occasional solar activity."],
   [244, "Ararus", "Ararus is most famous for its pink Esoneril tulip plantations and its weird exuberant forests."],
   [245, "Ara", "The world Ara is scourged by a evil disease."],
   [246, "Tianve", "The planet Tianve is reasonably noted for its inhabitants' exceptional loathing of food blenders and Zero-G cricket."],
   [247, "Quorte", "Quorte is well known for the Quortian tree wolf but scourged by dreadful solar activity."],
   [248, "Soladies", "This planet is fabled for its exciting Soladiesian evil brandy."],
   [249, "Maxeedso", "This world is mildly famous for its vast rain forests and the Maxeedsian tree wolf."],
   [250, "Xexedi", "The planet Xexedi is scourged by a deadly disease."],
   [251, "Xexeti", "This planet is notable for the Xexetian edible arts graduate and its great volcanoes."],
   [252, "Tiinlebi", "The planet Tiinlebi is most noted for the Tiinlebian mountain slug and its inhabitants' exceptional loathing of food blenders."],
   [253, "Rateedar", "Rateedar is cursed by dreadful civil war."],
   [254, "Onlema", "This world is plagued by frequent solar activity."],
   [255, "Orerve", "This planet is a dull place."]
   ])

(cstring/trim "v\n")
 

(defn test-planet-descriptions []
  (map-indexed (fn [index p]
          (let [name (last (take 7 (iterate generate-name (generate-name-start p))))
                goat-soup-string (egrammar/generate-goat-soup p name)
                original-planet (nth original-planet-descriptions index)]
            [(= (cstring/lower-case (cstring/trim (second original-planet)))
                (cstring/lower-case (cstring/trim name)))
             (= (cstring/lower-case (cstring/trim (nth original-planet 2)))
                (cstring/lower-case (cstring/trim goat-soup-string)))
             name
             goat-soup-string
             (nth original-planet 2)]))       
        planet-seed-list))

;; (count (filter (fn [v] (second v)) (test-planet-descriptions)))
;; (test-planet-descriptions)


;; => ([true
;;      false
;;      "TIBEDIED"
;;      "This planet is most fabled for Its Exciting Leopard Meat but scourged by deadly mountain bienceoids.\n"
;;      "This planet is most notable for Tibediedian Arnu brandy but ravaged by unpredictable solar activity."]
;;     [true
;;      false
;;      "QUBE"
;;      "Qube is reasonably well known for its vast mountains but ravaged by occasional civil war.\n"
;;      "Qube is reasonably well known for its great dense forests but scourged by deadly civil war."]
;;     [true
;;      false
;;      "LELEER"
;;      "The world leleer is very noted for its ancient  Plant  plantations but scourged by frequent civil war.\n"
;;      "The world Leleer is very noted for its pink Leleerian Itonthbi tulip plantations but plagued by deadly earthquakes."]
;;     [true
;;      false
;;      "BIARGE"
;;      "This world is very fabled for the biargeian edible poet.\n"
;;      "This world is very fabled for the Biargian edible poet."]
;;     [true
;;      false
;;      "XEQUERIN"
;;      "The world xequerin is very fabled for the xequerinian edible poet and its exotic night life.\n"
;;      "The world Xequerin is fabled for its weird volcanoes and the Xequerinian mountain lobstoid."]
;;     [true
;;      true
;;      "TIRAOR"
;;      "Tiraor is a revolting little planet.\n"
;;      "Tiraor is a revolting little planet."]
;;     [true
;;      false
;;      "RABEDIRA"
;;      "The planet rabedira is  well known for its inhabitants' ingrained love for poetry but beset by frequent earthquakes.\n"
;;      "The planet Rabedira is well known for its inhabitants' ancient loathing of sit coms but ravaged by dreadful civil war."]
;;     [true
;;      false
;;      "LAVE"
;;      "Lave is reasonably fabled for Its Unusual Fish Steak and its inhabitants' ingrained shyness.\n"
;;      "Lave is most famous for its vast rain forests and the Lavian tree grub."]
;;     [true
;;      false
;;      "ZAATXE"
;;      "This planet is mildly noted for the zaatxeian mountain batoid but beset by occasional solar activity.\n"
;;      "This planet is mildly noted for the Zaatxian deadly Atlenooid but ravaged by lethal lethal yaks."]
;;     [true
;;      false
;;      "DIUSREZA"
;;      "This planet is mildly fabled for its inhabitants' funny silliness but ravaged by deadly edible grubs.\n"
;;      "This planet is mildly fabled for its inhabitants' eccentric love for tourists but beset by deadly edible moths."]
;;     [true
;;      false
;;      "TEAATIS"
;;      "Teaatis is mildly well known for Lethal En Gargle Blasters.\n"
;;      "Teaatis is mildly well known for Teaatisian vicious brew."]
;;     [true
;;      false
;;      "RIINUS"
;;      "This world is reasonably fabled for Ice Karate and the riinusian edible poet.\n"
;;      "This world is mildly famous for its vast rain forests and the Riinusian tree grub."]
;;     [true
;;      true
;;      "ESBIZA"
;;      "The planet esbiza is most famous for its vast rain forests.\n"
;;      "The planet Esbiza is most famous for its vast rain forests."]
;;     [true
;;      false
;;      "ONTIMAXE"
;;      "This planet is reasonably famous for its inhabitants' exceptional loathing of food blenders.\n"
;;      "This planet is reasonably famous for its inhabitants' exceptional love for food blenders."]
;;     [true
;;      false
;;      "CEBETELA"
;;      "This world is most notable for its exciting Mud Tennis but beset by unpredictable solar activity.\n"
;;      "This world is most notable for its fabulous Cebetelian lethal brandy but scourged by killer mountain Esbionoids."]
;;     [true
;;      false
;;      "CEEDRA"
;;      "This planet is most fabled for its inhabitants' exceptional love for cuisine but cursed by frequent civil war.\n"
;;      "This planet is most fabled for its inhabitants' ingrained silliness but scourged by deadly civil war."]
;;     [true
;;      false
;;      "RIZALA"
;;      "The planet rizala is mildly notable for Rizalaian Deadly Brew.\n"
;;      "The planet Rizala is mildly notable for Rizalian lethal brandy."]
;;     [true
;;      false
;;      "ATRISO"
;;      "Atriso is most noted for the atrisoian mountain craboid and the atrisoian spotted bison.\n"
;;      "Atriso is mildly well known for its exotic cuisine and its inhabitants' ingrained silliness."]
;;     [true
;;      true
;;      "TEANREBI"
;;      "This planet is plagued by frequent earthquakes.\n"
;;      "This planet is plagued by frequent earthquakes."]
;;     [true
;;      false
;;      "AZAQU"
;;      "The planet azaqu is reasonably fabled for its unusual sit coms and its inhabitants' unusual silliness.\n"
;;      "The planet Azaqu is most famous for its pink oceans and Zero-G cricket."]
;;     [true
;;      true
;;      "RETILA"
;;      "This world is ravaged by occasional solar activity.\n"
;;      "This world is ravaged by occasional solar activity."]
;;     [true
;;      false
;;      "SOTIQU"
;;      "The planet sotiqu is very famous for Sotiquian Wolf Burgers but ravaged by dreadful earthquakes.\n"
;;      "The planet Sotiqu is famous for its exotic goat soup but ravaged by a killer disease."]
;;     [true
;;      false
;;      "INLEUS"
;;      "The world inleus is most famous for its pink Inleusian Itgeso Weed  plantations.\n"
;;      "The world Inleus is most famous for the Inleusian spotted wolf."]
;;     [true
;;      false
;;      "ONRIRA"
;;      "The world onrira is mildly noted for its strange dust clouds but beset by frequent solar activity.\n"
;;      "The world Onrira is mildly noted for the Onririan deadly Seoid but cursed by dreadful solar activity."]
;;     [true
;;      false
;;      "CEINZALA"
;;      "This planet is most notable for its exciting cuisine but beset by deadly solar activity.\n"
;;      "This planet is most notable for vicious Numaab juice but cursed by unpredictable solar activity."]
;;     [true
;;      true
;;      "BIISZA"
;;      "The planet biisza is most famous for its vast rain forests.\n"
;;      "The planet Biisza is most famous for its vast rain forests."]
;;     [true
;;      false
;;      "LEGEES"
;;      "This planet is most notable for its exotic casinos but plagued by frequent earthquakes.\n"
;;      "This planet is most notable for its exotic night life but ravaged by frequent earthquakes."]
;;     [true
;;      true
;;      "QUATOR"
;;      "The world quator is scourged by deadly edible arts graduates.\n"
;;      "The world Quator is scourged by deadly edible arts graduates."]
;;     [true
;;      false
;;      "AREXE"
;;      "The world arexe is very noted for its hoopy night life and its unusual sit coms.\n"
;;      "The world Arexe is fabled for its exciting sit coms and its inhabitants' ancient loathing of sit coms."]
;;     [true
;;      false
;;      "ATRABIIN"
;;      "Atrabiin is cursed by killer edible ouetonoids.\n"
;;      "Atrabiin is cursed by killer edible Nuatoids."]
;;     [true
;;      true
;;      "USANAT"
;;      "The world usanat is a boring world.\n"
;;      "The world Usanat is a boring world."]
;;     [true
;;      false
;;      "XEESLE"
;;      "The world xeesle is a boring world.\n"
;;      "The world Xeesle is a boring planet."]
;;     [true
;;      true
;;      "ORESEREN"
;;      "Oreseren is a revolting little planet.\n"
;;      "Oreseren is a revolting little planet."]
;;     [true
;;      false
;;      "INERA"
;;      "This planet is  noted for Its Exotic Fish Cutlet.\n"
;;      "This planet is noted for its exotic fish meat."]
;;     [true
;;      false
;;      "INUS"
;;      "This world is reasonably well known for the inusian vicious snail but cursed by a killer disease.\n"
;;      "This world is reasonably well known for the Inusian tree wolf but scourged by unpredictable earthquakes."]
;;     [true
;;      false
;;      "ISENCE"
;;      "The world isence is very famous for its hoopy cuisine but cursed by a evil disease.\n"
;;      "The world Isence is very famous for its unusual casinos but beset by a evil disease."]
;;     [true
;;      false
;;      "REESDICE"
;;      "The world reesdice is reasonably famous for the reesdiceian evil fish.\n"
;;      "The world Reesdice is reasonably famous for the Reesdician deadly lobstoid."]
;;     [true
;;      false
;;      "TEREA"
;;      "This world is very fabled for the tereaian mountain slug.\n"
;;      "This world is very fabled for the Terian edible poet."]
;;     [true
;;      true
;;      "ORGETIBE"
;;      "This planet is a dull world.\n"
;;      "This planet is a dull world."]
;;     [true
;;      false
;;      "REORTE"
;;      "This planet is mildly fabled for its inhabitants' wierd silliness but ravaged by a lethal disease.\n"
;;      "This planet is mildly fabled for its inhabitants' eccentric love for tourists but plagued by deadly earthquakes."]
;;     [true
;;      false
;;      "QUQUOR"
;;      "The planet ququor is mildly notable for Killer Xege Juice.\n"
;;      "The planet Ququor is mildly well known for its exotic cuisine."]
;;     [true
;;      true
;;      "GEINONA"
;;      "This world is ravaged by unpredictable solar activity.\n"
;;      "This world is ravaged by unpredictable solar activity."]
;;     [true
;;      false
;;      "ANARLAQU"
;;      "This world is reasonably noted for its inhabitants' exceptional love for tourists and the anarlaquian mountain lobstoid.\n"
;;      "This world is mildly famous for its hoopy night life and its exotic night life."]
;;     [true
;;      true
;;      "ORESRI"
;;      "The planet oresri is cursed by dreadful civil war.\n"
;;      "The planet Oresri is cursed by dreadful civil war."]
;;     [true
;;      false
;;      "ESESLA"
;;      "This planet is  noted for Mud Hockey.\n"
;;      "This planet is noted for Zero-G hockey."]
;;     [true
;;      true
;;      "SOCELAGE"
;;      "This planet is reasonably noted for Its Exotic Goat Meat.\n"
;;      "This planet is reasonably noted for its exotic goat meat."]
;;     [true
;;      false
;;      "RIEDQUAT"
;;      "This planet is most notable for its exciting cuisine but beset by frequent civil war.\n"
;;      "This planet is most notable for its fabulous cuisine but beset by occasional civil war."]
;;     [true
;;      false
;;      "GEREGE"
;;      "The world gerege is reasonably famous for the geregeian spotted wolf.\n"
;;      "The world Gerege is reasonably famous for the Geregian spotted wolf."]
;;     [true
;;      false
;;      "USLE"
;;      "This world is mildly notable for Its Fabulous Goat Soup and its pink ice bergs.\n"
;;      "This world is very notable for the Uslian tree ant and its inhabitants' exceptional loathing of sit coms."]
;;     [true
;;      true
;;      "MALAMA"
;;      "The planet malama is mildly well known for its exotic cuisine.\n"
;;      "The planet Malama is mildly well known for its exotic cuisine."]
;;     [true
;;      true
;;      "AESBION"
;;      "The planet aesbion is cursed by dreadful civil war.\n"
;;      "The planet Aesbion is cursed by dreadful civil war."]
;;     [true
;;      true
;;      "ALAZA"
;;      "The world alaza is scourged by a evil disease.\n"
;;      "The world Alaza is scourged by a evil disease."]
;;     [true
;;      true
;;      "XEAQU"
;;      "The world xeaqu is a dull place.\n"
;;      "The world Xeaqu is a dull place."]
;;     [true
;;      false
;;      "RAOROR"
;;      "This world is very fabled for its unusual mountains.\n"
;;      "This world is very fabled for its weird volcanoes."]
;;     [true
;;      false
;;      "ORORQU"
;;      "The planet ororqu is  well known for its inhabitants' eccentric mating traditions but beset by unpredictable solar activity.\n"
;;      "The planet Ororqu is well known for its inhabitants' ancient mating traditions but ravaged by unpredictable solar activity."]
;;     [true
;;      false
;;      "LEESTI"
;;      "The planet leesti is very famous for Leestiian Killer Brandy and Its Unusual Monkey Burgers.\n"
;;      "The planet Leesti is reasonably fabled for Zero-G cricket and Leestian evil juice."]
;;     [true
;;      false
;;      "GEISGEZA"
;;      "This planet is mildly fabled for the geisgezaian spotted monkey and its hoopy cuisine.\n"
;;      "This planet is notable for its unusual oceans and the Geisgezian mountain slug."]
;;     [true
;;      true
;;      "ZAINLABI"
;;      "This world is ravaged by unpredictable civil war.\n"
;;      "This world is ravaged by unpredictable civil war."]
;;     [true
;;      false
;;      "USCELA"
;;      "The world uscela is a dull world.\n"
;;      "The world Uscela is a boring world."]
;;     [true
;;      false
;;      "ISVEVE"
;;      "The planet isveve is  well known for its inhabitants' eccentric mating traditions and  Brew.\n"
;;      "The planet Isveve is reasonably noted for its inhabitants' eccentric shyness and its inhabitants' eccentric shyness."]
;;     [true
;;      false
;;      "TIORANIN"
;;      "This world is most notable for Tioraninian Deadly Water but beset by dreadful solar activity.\n"
;;      "This world is most notable for Tioraninian vicious brew but ravaged by unpredictable civil war."]
;;     [true
;;      false
;;      "LEARORCE"
;;      "Learorce is reasonably notable for its vast mountains but ravaged by unpredictable civil war.\n"
;;      "Learorce is reasonably notable for its great dense forests but scourged by deadly edible poets."]
;;     [true
;;      false
;;      "ESUSTI"
;;      "This world is most famous for its vast rock formations and its vast mountains.\n"
;;      "This world is very well known for its inhabitants' ancient loathing of discos and the Esustian spotted cat."]
;;     [true
;;      false
;;      "USUSOR"
;;      "This planet is mildly well known for its fabulous night life and its unusual tropical forests.\n"
;;      "This planet is very notable for its inhabitants' weird shyness and the Ususorian edible poet."]
;;     [true
;;      false
;;      "MAREGEIS"
;;      "This world is  fabled for its ancient Maregeisian  Tulip  plantations.\n"
;;      "This world is fabled for its ancient Maregeisian Onbidi tulip plantations."]
;;     [true
;;      true
;;      "AATE"
;;      "The world aate is scourged by killer mountain lobstoids.\n"
;;      "The world Aate is scourged by killer mountain lobstoids."]
;;     [true
;;      false
;;      "SORI"
;;      "The world sori is scourged by a evil disease.\n"
;;      "The world Sori is beset by a evil disease."]
;;     [true
;;      true
;;      "CEMAVE"
;;      "The world cemave is beset by dreadful earthquakes.\n"
;;      "The world Cemave is beset by dreadful earthquakes."]
;;     [true
;;      false
;;      "ARUSQUDI"
;;      "This world is very fabled for its peculiar dust clouds.\n"
;;      "This world is very fabled for its unusual oceans."]
;;     [true
;;      true
;;      "EREDVE"
;;      "This planet is beset by a evil disease.\n"
;;      "This planet is beset by a evil disease."]
;;     [true
;;      false
;;      "REGEATGE"
;;      "Regeatge is reasonably well known for its vast mountains but ravaged by frequent solar activity.\n"
;;      "Regeatge is reasonably well known for its great dense forests but scourged by frequent civil war."]
;;     [true
;;      false
;;      "EDINSO"
;;      "This planet is mildly noted for its pink oceans but scourged by deadly solar activity.\n"
;;      "This planet is mildly noted for its pink Edinsian Ge Bemaarleweed plantations but ravaged by vicious mountain monkeys."]
;;     [true
;;      true
;;      "RA"
;;      "The world ra is beset by deadly earthquakes.\n"
;;      "The world Ra is beset by deadly earthquakes."]
;;     [true
;;      false
;;      "ARONAR"
;;      "Aronar is reasonably notable for its peculiar ice bergs and its inhabitants' exceptional silliness.\n"
;;      "Aronar is most famous for the Aronarian deadly goat and its hoopy casinos."]
;;     [true
;;      false
;;      "ARRAESSO"
;;      "This planet is mildly fabled for the arraessoian spotted monkey and its hoopy cuisine.\n"
;;      "This planet is notable for its unusual oceans and its inhabitants' exceptional loathing of food blenders."]
;;     [true
;;      true
;;      "CEVEGE"
;;      "This world is a revolting dump.\n"
;;      "This world is a revolting dump."]
;;     [true
;;      false
;;      "ORTEVE"
;;      "This world is  fabled for its fabulous cuisine.\n"
;;      "This world is fabled for its fabulous vicious Ougeza juice."]
;;     [true
;;      false
;;      "GEERRA"
;;      "This planet is reasonably noted for Its Fabulous Goat Soup.\n"
;;      "This planet is reasonably noted for its exotic goat soup."]
;;     [true
;;      true
;;      "SOINUSTE"
;;      "This planet is beset by deadly earthquakes.\n"
;;      "This planet is beset by deadly earthquakes."]
;;     [true
;;      false
;;      "ERLAGE"
;;      "This world is reasonably well known for the erlageian spotted arts graduate but cursed by frequent earthquakes.\n"
;;      "This world is reasonably well known for the Erlagian tree ant but cursed by vicious mountain goats."]
;;     [true
;;      true
;;      "XEAAN"
;;      "This world is ravaged by unpredictable civil war.\n"
;;      "This world is ravaged by unpredictable civil war."]
;;     [true
;;      true
;;      "VEIS"
;;      "The planet veis is a boring world.\n"
;;      "The planet Veis is a boring world."]
;;     [true
;;      true
;;      "ENSOREUS"
;;      "This planet is a tedious little planet.\n"
;;      "This planet is a tedious little planet."]
;;     [true
;;      true
;;      "RIVEIS"
;;      "The world riveis is most well known for its hoopy casinos.\n"
;;      "The world Riveis is most well known for its hoopy casinos."]
;;     [true
;;      true
;;      "BIVEA"
;;      "This planet is plagued by frequent solar activity.\n"
;;      "This planet is plagued by frequent solar activity."]
;;     [true
;;      false
;;      "ERMASO"
;;      "This planet is mildly notable for Its Exciting Cat Cutlet and its pink rock formations.\n"
;;      "This planet is very notable for the Ermasian edible grub and the Ermasian tree ant."]
;;     [true
;;      true
;;      "VELETE"
;;      "Velete is a revolting dump.\n"
;;      "Velete is a revolting dump."]
;;     [true
;;      true
;;      "ENGEMA"
;;      "The world engema is beset by a evil disease.\n"
;;      "The world Engema is beset by a evil disease."]
;;     [true
;;      true
;;      "ATRIENXE"
;;      "Atrienxe is an unremarkable dump.\n"
;;      "Atrienxe is an unremarkable dump."]
;;     [true
;;      true
;;      "BEUSRIOR"
;;      "The world beusrior is a dull world.\n"
;;      "The world Beusrior is a dull world."]
;;     [true
;;      true
;;      "ONTIAT"
;;      "The planet ontiat is scourged by a evil disease.\n"
;;      "The planet Ontiat is scourged by a evil disease."]
;;     [true
;;      true
;;      "ATARZA"
;;      "This world is plagued by occasional solar activity.\n"
;;      "This world is plagued by occasional solar activity."]
;;     [true
;;      false
;;      "ARAZAES"
;;      "This planet is mildly notable for Mud Tennis and its pink tropical forests.\n"
;;      "This planet is very notable for the Arazaesian tree ant and Arazaesian wolf meat."]
;;     [true
;;      false
;;      "XEERANRE"
;;      "Xeeranre is cursed by killer edible stdioids.\n"
;;      "Xeeranre is cursed by killer mountain Reetaboids."]
;;     [true
;;      true
;;      "QUZADI"
;;      "Quzadi is cursed by dreadful civil war.\n"
;;      "Quzadi is cursed by dreadful civil war."]
;;     [true
;;      false
;;      "ISTI"
;;      "The planet isti is  well known for its inhabitants' ingrained loathing of tourists and Istiian Ar Brew.\n"
;;      "The planet Isti is reasonably noted for its inhabitants' eccentric shyness and Zero-G hockey."]
;;     [true
;;      false
;;      "DIGEBITI"
;;      "Digebiti is cursed by killer mountain lobstoids.\n"
;;      "Digebiti is cursed by killer mountain Seoids."]
;;     [true
;;      false
;;      "LEONED"
;;      "Leoned is reasonably well known for the leonedian vicious yak but cursed by vicious mountain goats.\n"
;;      "Leoned is reasonably well known for the Leonedian tree snake but scourged by unpredictable earthquakes."]
;;     [true
;;      true
;;      "ENZAER"
;;      "Enzaer is a revolting dump.\n"
;;      "Enzaer is a revolting dump."]
;;     [true
;;      true
;;      "TERAED"
;;      "Teraed is an unremarkable dump.\n"
;;      "Teraed is an unremarkable dump."]
;;     [true
;;      false
;;      "VETITICE"
;;      "This world is most famous for its vast mountains and its vast Vetiticeian Vicious Banana  plantations.\n"
;;      "This world is very well known for Vetitician lethal brandy and its great parking meters."]
;;     [true
;;      false
;;      "LAENIN"
;;      "The planet laenin is  famous for its inhabitants' ingrained love for discos but beset by occasional earthquakes.\n"
;;      "The planet Laenin is famous for its inhabitants' ancient loathing of sit coms but cursed by a killer disease."]
;;     [true
;;      false
;;      "BERAANXE"
;;      "The world beraanxe is  well known for the beraanxeian mountain bison and Beraanxeian Moth Brandy.\n"
;;      "The world Beraanxe is reasonably noted for its inhabitants' exceptional love for tourists and its unusual oceans."]
;;     [true
;;      true
;;      "ATAGE"
;;      "Atage is an unremarkable planet.\n"
;;      "Atage is an unremarkable planet."]
;;     [true
;;      false
;;      "VEISTI"
;;      "The planet veisti is  well known for its inhabitants' ingrained loathing of food blenders and its inhabitants' ancient silliness.\n"
;;      "The planet Veisti is reasonably noted for its inhabitants' eccentric shyness and Zero-G cricket."]
;;     [true
;;      false
;;      "ZAERLA"
;;      "The planet zaerla is mildly well known for its exotic night life.\n"
;;      "The planet Zaerla is mildly well known for its exotic cuisine."]
;;     [true
;;      true
;;      "ESREDICE"
;;      "The world esredice is a boring planet.\n"
;;      "The world Esredice is a boring planet."]
;;     [true
;;      true
;;      "BEOR"
;;      "Beor is an unremarkable dump.\n"
;;      "Beor is an unremarkable dump."]
;;     [true
;;      false
;;      "ORSO"
;;      "The world orso is very famous for its hoopy cuisine and Vacuum Tennis.\n"
;;      "The world Orso is reasonably fabled for its exciting sit coms and its inhabitants' exceptional love for food blenders."]
;;     [true
;;      false
;;      "USATQURA"
;;      "This planet is reasonably famous for its inhabitants' exceptional loathing of casinos.\n"
;;      "This planet is reasonably famous for its inhabitants' exceptional loathing of sit coms."]
;;     [true
;;      false
;;      "ERBITI"
;;      "The world erbiti is most well known for its great tropical forests.\n"
;;      "The world Erbiti is most well known for its great dense forests."]
;;     [true
;;      true
;;      "REINEN"
;;      "This planet is a tedious little planet.\n"
;;      "This planet is a tedious little planet."]
;;     [true
;;      false
;;      "ININBI"
;;      "The world ininbi is reasonably famous for its inhabitants' ancient loathing of casinos.\n"
;;      "The world Ininbi is reasonably famous for its inhabitants' exceptional loathing of casinos."]
;;     [true
;;      false
;;      "ERLAZA"
;;      "The world erlaza is mildly noted for its unusual mountains but beset by occasional solar activity.\n"
;;      "The world Erlaza is mildly noted for its ancient mountains but plagued by a lethal disease."]
;;     [true
;;      false
;;      "CELABILE"
;;      "The planet celabile is reasonably notable for its great Inhabitant Tulip  plantations and its inhabitants' eccentric loathing of night life.\n"
;;      "The planet Celabile is most famous for the Celabilian evil poet and Zero-G hockey."]
;;     [true
;;      false
;;      "RIBISO"
;;      "This planet is  fabled for its exciting Zero-g cricket.\n"
;;      "This planet is fabled for its exciting vacuum cricket."]
;;     [true
;;      false
;;      "QUDIRA"
;;      "The world qudira is very famous for its hoopy cuisine and Zero-g karate.\n"
;;      "The world Qudira is reasonably fabled for Qudirian Ouarma gargle blasters and the Qudirian evil talking treeoid."]
;;     [true
;;      true
;;      "ISDIBI"
;;      "The world isdibi is scourged by deadly tree ants.\n"
;;      "The world Isdibi is scourged by deadly tree ants."]
;;     [true
;;      false
;;      "GEQURE"
;;      "This world is reasonably well known for the gequreian spotted poet but cursed by a lethal disease.\n"
;;      "This world is reasonably well known for the Gequrian tree ant but ravaged by dreadful civil war."]
;;     [true
;;      false
;;      "RARERE"
;;      "The planet rarere is mildly notable for Rarereian Deadly Brew.\n"
;;      "The planet Rarere is mildly notable for Rarerian lethal brandy."]
;;     [true
;;      true
;;      "AERATER"
;;      "Aerater is a revolting little planet.\n"
;;      "Aerater is a revolting little planet."]
;;     [true
;;      false
;;      "ATBEVETE"
;;      "The planet atbevete is mildly well known for Vicious Zaal Juice.\n"
;;      "The planet Atbevete is mildly well known for killer Ou gargle blasters."]
;;     [true
;;      true
;;      "BIORIS"
;;      "Bioris is very fabled for the biorisian edible poet.\n"
;;      "Bioris is very fabled for the Biorisian edible poet."]
;;     [true
;;      false
;;      "RAALE"
;;      "This world is very fabled for the raaleian edible poet.\n"
;;      "This world is very fabled for the Raalian edible poet."]
;;     [true
;;      true
;;      "TIONISLA"
;;      "This world is very notable for its inhabitants' ingrained shyness.\n"
;;      "This world is very notable for its inhabitants' ingrained shyness."]
;;     [true
;;      true
;;      "ENCERESO"
;;      "Encereso is cursed by dreadful civil war.\n"
;;      "Encereso is cursed by dreadful civil war."]
;;     [true
;;      false
;;      "ANERBE"
;;      "The world anerbe is very noted for its exciting sit coms and its exciting casinos.\n"
;;      "The world Anerbe is reasonably fabled for its exciting vacuum karate and its great volcanoes."]
;;     [true
;;      false
;;      "GELAED"
;;      "The planet gelaed is very noted for its pink oceans but scourged by deadly solar activity.\n"
;;      "The planet Gelaed is very noted for its pink Gelaedian Ines Soweed plantations but ravaged by unpredictable civil war."]
;;     [true
;;      false
;;      "ONUSORLE"
;;      "This world is most famous for its pink tropical forests and its vast rock formations.\n"
;;      "This world is mildly well known for Onusorlian vicious brew and Onusorlian wolf cutlet."]
;;     [true
;;      true
;;      "ZAONCE"
;;      "This planet is a tedious place.\n"
;;      "This planet is a tedious place."]
;;     [true
;;      true
;;      "DIQUER"
;;      "The world diquer is a dull place.\n"
;;      "The world Diquer is a dull place."]
;;     [true
;;      false
;;      "ZADIES"
;;      "The planet zadies is  famous for its inhabitants' peculiar shyness but plagued by deadly solar activity.\n"
;;      "The planet Zadies is famous for its inhabitants' exceptional love for food blenders but scourged by dreadful solar activity."]
;;     [true
;;      false
;;      "ENTIZADI"
;;      "The planet entizadi is  famous for its inhabitants' ingrained love for discos but plagued by unpredictable earthquakes.\n"
;;      "The planet Entizadi is famous for its inhabitants' exceptional love for food blenders but scourged by dreadful solar activity."]
;;     [true
;;      false
;;      "ESANBE"
;;      "Esanbe is  famous for its inhabitants' ingrained loathing of food blenders but beset by evil tree fishs.\n"
;;      "Esanbe is famous for its inhabitants' ancient loathing of casinos but plagued by deadly earthquakes."]
;;     [true
;;      true
;;      "USRALAAT"
;;      "This planet is plagued by deadly earthquakes.\n"
;;      "This planet is plagued by deadly earthquakes."]
;;     [true
;;      false
;;      "ANLERE"
;;      "Anlere is reasonably well known for the anlereian deadly beast but scourged by a lethal disease.\n"
;;      "Anlere is reasonably well known for the Anlerian spotted shrew but plagued by evil tree leopards."]
;;     [true
;;      false
;;      "TEVERI"
;;      "The world teveri is very famous for its hoopy cuisine and Vacuum Polo.\n"
;;      "The world Teveri is reasonably fabled for Teverian evil juice and its inhabitants' ingrained shyness."]
;;     [true
;;      false
;;      "SOTIERA"
;;      "The world sotiera is mildly fabled for the sotieraian tree wolf but plagued by unpredictable earthquakes.\n"
;;      "The world Sotiera is mildly fabled for the Sotierian mountain poet but cursed by unpredictable earthquakes."]
;;     [true
;;      true
;;      "EDEDLEEN"
;;      "The planet ededleen is mildly well known for its exotic cuisine.\n"
;;      "The planet Ededleen is mildly well known for its exotic cuisine."]
;;     [true
;;      false
;;      "INONRI"
;;      "This world is most famous for the inonriian killer wasp and its pink Inonriian Diin Banana  plantations.\n"
;;      "This world is very well known for Inonrian wolf meat and its weird volcanoes."]
;;     [true
;;      false
;;      "ESBEUS"
;;      "The world esbeus is mildly noted for its strange mountains but beset by dreadful civil war.\n"
;;      "The world Esbeus is mildly noted for its ancient mountains but plagued by frequent earthquakes."]
;;     [true
;;      true
;;      "LERELACE"
;;      "This planet is a dull place.\n"
;;      "This planet is a dull place."]
;;     [true
;;      false
;;      "ESZARAXE"
;;      "The planet eszaraxe is reasonably notable for its fabulous night life and its inhabitants' exceptional mating traditions.\n"
;;      "The planet Eszaraxe is most famous for the Eszaraxian spotted shrew and the Eszaraxian mountain poet."]
;;     [true
;;      false
;;      "ANBEEN"
;;      "Anbeen is reasonably notable for its vast Anbeenian Esil Aberenweed  plantations but ravaged by deadly solar activity.\n"
;;      "Anbeen is reasonably notable for its great tropical forests but cursed by dreadful solar activity."]
;;     [true
;;      true
;;      "BIORLE"
;;      "The world biorle is a dull world.\n"
;;      "The world Biorle is a dull world."]
;;     [true
;;      false
;;      "ANISOR"
;;      "This planet is most well known for its vast Anisorian  Corn  plantations and its great oceans.\n"
;;      "This planet is very well known for its inhabitants' ancient mating traditions and its inhabitants' ancient loathing of casinos."]
;;     [true
;;      false
;;      "USRAREMA"
;;      "This world is very fabled for the usraremaian edible poet.\n"
;;      "This world is very notable for the Usraremian edible poet."]
;;     [true
;;      false
;;      "DISO"
;;      "This planet is mildly noted for its ancient Disoian Killer Corn  plantations but beset by vicious spotted snails.\n"
;;      "This planet is mildly noted for its ancient Ma corn plantations but beset by frequent solar activity."]
;;     [true
;;      false
;;      "RIRAES"
;;      "The world riraes is very fabled for its great impenetrable forests and its exotic cuisine.\n"
;;      "The world Riraes is fabled for its weird rock formations and its pink oceans."]
;;     [true
;;      false
;;      "ORRIRA"
;;      "The planet orrira is cursed by killer edible poets.\n"
;;      "The planet Orrira is cursed by killer edible talking treeoids."]
;;     [true
;;      false
;;      "XEER"
;;      "This world is most famous for its ancient oceans and its pink oceans.\n"
;;      "This world is very well known for Xeerian wolf meat and its fabulous cuisine."]
;;     [true
;;      true
;;      "CEESXE"
;;      "The world ceesxe is most well known for its vast rain forests.\n"
;;      "The world Ceesxe is most well known for its vast rain forests."]
;;     [true
;;      true
;;      "ISATRE"
;;      "The world isatre is a boring planet.\n"
;;      "The world Isatre is a boring planet."]
;;     [true
;;      false
;;      "AONA"
;;      "This world is most famous for its vast mountains and its vast oceans.\n"
;;      "This world is very well known for Aonian lethal brandy and its great volcanoes."]
;;     [true
;;      false
;;      "ISINOR"
;;      "This world is very fabled for its peculiar dust clouds.\n"
;;      "This world is very fabled for its unusual oceans."]
;;     [true
;;      false
;;      "USZAA"
;;      "The planet uszaa is  well known for its inhabitants' exceptional silliness and Uszaaian Age Gargle Blasters.\n"
;;      "The planet Uszaa is reasonably noted for its inhabitants' eccentric love for tourists and the Uszaian tree grub."]
;;     [true
;;      false
;;      "AANBIAT"
;;      "This planet is  fabled for its ancient Aanbiatian Ma Banana  plantations.\n"
;;      "This planet is fabled for its ancient Aanbiatian Noalin banana plantations."]
;;     [true
;;      false
;;      "BEMAERA"
;;      "Bemaera is  notable for its great oceans and its inhabitants' eccentric mating traditions.\n"
;;      "Bemaera is most noted for the Bemaerian deadly Xesooid and its inhabitants' unusual silliness."]
;;     [true
;;      true
;;      "ININES"
;;      "This world is a tedious place.\n"
;;      "This world is a tedious place."]
;;     [true
;;      false
;;      "EDZAON"
;;      "This world is most notable for its exciting Its Unusual Leopard Soup but beset by occasional earthquakes.\n"
;;      "This world is most notable for Edzaonian lethal water but plagued by occasional solar activity."]
;;     [true
;;      true
;;      "LERITEAN"
;;      "The planet leritean is mildly well known for its exotic cuisine.\n"
;;      "The planet Leritean is mildly well known for its exotic cuisine."]
;;     [true
;;      true
;;      "VEALE"
;;      "The world veale is most well known for its vast dense forests.\n"
;;      "The world Veale is most well known for its vast dense forests."]
;;     [true
;;      false
;;      "EDLE"
;;      "Edle is  famous for its inhabitants' peculiar shyness but plagued by frequent civil war.\n"
;;      "Edle is famous for its inhabitants' exceptional love for food blenders but scourged by frequent civil war."]
;;     [true
;;      true
;;      "ANLAMA"
;;      "This world is a tedious little planet.\n"
;;      "This world is a tedious little planet."]
;;     [true
;;      false
;;      "RIBILEBI"
;;      "The planet ribilebi is reasonably fabled for Mud Tennis and its inhabitants' unusual shyness.\n"
;;      "The planet Ribilebi is most famous for its vast oceans and its fabulous goat soup."]
;;     [true
;;      true
;;      "RELAES"
;;      "This world is a tedious place.\n"
;;      "This world is a tedious place."]
;;     [true
;;      true
;;      "DIZAONER"
;;      "Dizaoner is ravaged by unpredictable solar activity.\n"
;;      "Dizaoner is ravaged by unpredictable solar activity."]
;;     [true
;;      true
;;      "RAZAAR"
;;      "The world razaar is a dull place.\n"
;;      "The world Razaar is a dull place."]
;;     [true
;;      false
;;      "ENONLA"
;;      "Enonla is ravaged by unpredictable civil war.\n"
;;      "Enonla is ravaged by dreadful civil war."]
;;     [true
;;      true
;;      "ISANLEQU"
;;      "This planet is beset by a evil disease.\n"
;;      "This planet is beset by a evil disease."]
;;     [true
;;      false
;;      "TIBECEA"
;;      "Tibecea is very fabled for the tibeceaian edible poet.\n"
;;      "Tibecea is very fabled for the Tibecian edible poet."]
;;     [true
;;      false
;;      "SOTERA"
;;      "Sotera is mildly notable for Soteraian Deadly Brew.\n"
;;      "Sotera is mildly notable for Soterian lethal brandy."]
;;     [true
;;      false
;;      "ESVEOR"
;;      "Esveor is reasonably fabled for its exciting sit coms and the esveorian edible fish.\n"
;;      "Esveor is mildly famous for its pink oceans and Zero-G hockey."]
;;     [true
;;      false
;;      "ESTEONBI"
;;      "This planet is mildly fabled for its inhabitants' ancient loathing of poetry but cursed by occasional earthquakes.\n"
;;      "This planet is mildly fabled for its inhabitants' ingrained shyness but cursed by unpredictable solar activity."]
;;     [true
;;      false
;;      "XEESENRI"
;;      "Xeesenri is mildly notable for its inhabitants' ingrained silliness.\n"
;;      "Xeesenri is mildly notable for its inhabitants' weird shyness."]
;;     [true
;;      false
;;      "ORESLE"
;;      "This world is reasonably notable for its great oceans but ravaged by a evil disease.\n"
;;      "This world is reasonably notable for its great volcanoes but ravaged by a vicious disease."]
;;     [true
;;      true
;;      "ERVEIN"
;;      "Ervein is a revolting little planet.\n"
;;      "Ervein is a revolting little planet."]
;;     [true
;;      true
;;      "LARAIS"
;;      "This world is a revolting dump.\n"
;;      "This world is a revolting dump."]
;;     [true
;;      true
;;      "ANXEBIZA"
;;      "The planet anxebiza is an unremarkable dump.\n"
;;      "The planet Anxebiza is an unremarkable dump."]
;;     [true
;;      true
;;      "DIEDAR"
;;      "This world is ravaged by dreadful civil war.\n"
;;      "This world is ravaged by dreadful civil war."]
;;     [true
;;      true
;;      "ENINRE"
;;      "The planet eninre is cursed by deadly civil war.\n"
;;      "The planet Eninre is cursed by deadly civil war."]
;;     [true
;;      false
;;      "BIBE"
;;      "This world is most fabled for Mud Tennis but scourged by occasional earthquakes.\n"
;;      "This world is most fabled for Bibian lethal brandy but beset by a evil disease."]
;;     [true
;;      false
;;      "DIQUXE"
;;      "This planet is mildly noted for its strange parking meters but beset by frequent solar activity.\n"
;;      "This planet is mildly noted for its ancient mountains but plagued by frequent earthquakes."]
;;     [true
;;      true
;;      "SORACE"
;;      "Sorace is cursed by deadly civil war.\n"
;;      "Sorace is cursed by deadly civil war."]
;;     [true
;;      true
;;      "ANXEONIS"
;;      "The planet anxeonis is most famous for its vast rain forests.\n"
;;      "The planet Anxeonis is most famous for its vast rain forests."]
;;     [true
;;      false
;;      "RIANTIAT"
;;      "This planet is mildly notable for its inhabitants' eccentric silliness and its exciting sit coms.\n"
;;      "This planet is notable for the Riantiatian edible grub and the Riantiatian spotted wolf."]
;;     [true
;;      true
;;      "ZARECE"
;;      "This planet is a tedious place.\n"
;;      "This planet is a tedious place."]
;;     [true
;;      true
;;      "MAESIN"
;;      "The planet maesin is an unremarkable dump.\n"
;;      "The planet Maesin is an unremarkable dump."]
;;     [true
;;      false
;;      "TIBIONIS"
;;      "Tibionis is  notable for its great exuberant forests and its inhabitants' exceptional shyness.\n"
;;      "Tibionis is most noted for the Tibionisian deadly goat and its vast rain forests."]
;;     [true
;;      false
;;      "GELEGEUS"
;;      "Gelegeus is mildly notable for Gelegeusian Deadly Brew.\n"
;;      "Gelegeus is mildly notable for Gelegeusian Bidialst brandy."]
;;     [true
;;      false
;;      "DIORA"
;;      "The planet diora is a boring planet.\n"
;;      "The planet Diora is an unremarkable planet."]
;;     [true
;;      true
;;      "RIGETI"
;;      "Rigeti is a revolting dump.\n"
;;      "Rigeti is a revolting dump."]
;;     [true
;;      false
;;      "BEGEABI"
;;      "Begeabi is very notable for its inhabitants' wierd silliness.\n"
;;      "Begeabi is very notable for its inhabitants' weird silliness."]
;;     [true
;;      false
;;      "ORRERE"
;;      "Orrere is mildly well known for Lethal On Gargle Blasters.\n"
;;      "Orrere is mildly well known for Orrerian vicious brew."]
;;     [true
;;      false
;;      "BETI"
;;      "This planet is very fabled for the betiian edible wasp and its exotic night life.\n"
;;      "This planet is fabled for its weird volcanoes and the Betian mountain lobstoid."]
;;     [true
;;      false
;;      "GERETE"
;;      "This world is most fabled for Its Unusual Fish Steak but cursed by evil lethal craboids.\n"
;;      "This world is most fabled for Zero-G cricket but cursed by unpredictable solar activity."]
;;     [true
;;      true
;;      "QUCERERE"
;;      "This planet is a tedious place.\n"
;;      "This planet is a tedious place."]
;;     [true
;;      true
;;      "XEONER"
;;      "The world xeoner is a dull world.\n"
;;      "The world Xeoner is a dull world."]
;;     [true
;;      true
;;      "XEZAOR"
;;      "The world xezaor is most well known for its hoopy casinos.\n"
;;      "The world Xezaor is most well known for its hoopy casinos."]
;;     [true
;;      false
;;      "RITILA"
;;      "The world ritila is very famous for its hoopy cuisine but ravaged by a evil disease.\n"
;;      "The world Ritila is very famous for its hoopy casinos but beset by a evil disease."]
;;     [true
;;      true
;;      "EDORTE"
;;      "The planet edorte is an unremarkable dump.\n"
;;      "The planet Edorte is an unremarkable dump."]
;;     [true
;;      false
;;      "ZAALELA"
;;      "This world is  noted for Its Fabulous Goat Burgers.\n"
;;      "This world is noted for its fabulous goat soup."]
;;     [true
;;      false
;;      "BIISORTE"
;;      "This world is most notable for Biisorteian Deadly Water but beset by dreadful solar activity.\n"
;;      "This world is most notable for its fabulous Biisortian lethal water but beset by a lethal disease."]
;;     [true
;;      true
;;      "BEESOR"
;;      "This world is plagued by deadly earthquakes.\n"
;;      "This world is plagued by deadly earthquakes."]
;;     [true
;;      true
;;      "ORESQU"
;;      "Oresqu is mildly notable for its inhabitants' unusual mating traditions.\n"
;;      "Oresqu is mildly notable for its inhabitants' unusual mating traditions."]
;;     [true
;;      true
;;      "XEQUQUTI"
;;      "This planet is beset by dreadful earthquakes.\n"
;;      "This planet is beset by dreadful earthquakes."]
;;     [true
;;      false
;;      "MAISES"
;;      "Maises is reasonably notable for its exciting Ice Karate but beset by unpredictable earthquakes.\n"
;;      "Maises is reasonably notable for its fabulous Maisesian lethal water but beset by a lethal disease."]
;;     [true
;;      false
;;      "BIERLE"
;;      "The planet bierle is reasonably notable for its great oceans and its inhabitants' eccentric silliness.\n"
;;      "The planet Bierle is most famous for the Bierlian deadly Inoid and its inhabitants' ingrained silliness."]
;;     [true
;;      true
;;      "ARZASO"
;;      "Arzaso is an unremarkable planet.\n"
;;      "Arzaso is an unremarkable planet."]
;;     [true
;;      true
;;      "TEEN"
;;      "Teen is cursed by deadly civil war.\n"
;;      "Teen is cursed by deadly civil war."]
;;     [true
;;      false
;;      "RIREDI"
;;      "This world is very fabled for the rirediian mountain illeoid.\n"
;;      "This world is very fabled for the Riredian mountain slug."]
;;     [true
;;      true
;;      "TEORGE"
;;      "This planet is a tedious little planet.\n"
;;      "This planet is a tedious little planet."]
;;     [true
;;      false
;;      "VEBEGE"
;;      "The world vebege is mildly fabled for the vebegeian edible snail but plagued by deadly civil war.\n"
;;      "The world Vebege is mildly fabled for the Vebegian mountain lobstoid but beset by deadly solar activity."]
;;     [true
;;      false
;;      "XEENLE"
;;      "This planet is mildly noted for its strange mountains but beset by dreadful civil war.\n"
;;      "This planet is mildly noted for its ancient mountains but plagued by a lethal disease."]
;;     [true
;;      true
;;      "ARXEZA"
;;      "The world arxeza is beset by dreadful earthquakes.\n"
;;      "The world Arxeza is beset by dreadful earthquakes."]
;;     [true
;;      false
;;      "EDREOR"
;;      "The world edreor is very noted for its ancient oceans and Vicious Za Gargle Blasters.\n"
;;      "The world Edreor is reasonably fabled for its fabulous killer Sese juice and its ancient Edreorian Esnu corn plantations."]
;;     [true
;;      true
;;      "ESGEREAN"
;;      "This planet is plagued by occasional solar activity.\n"
;;      "This planet is plagued by occasional solar activity."]
;;     [true
;;      false
;;      "DITIZA"
;;      "The planet ditiza is very famous for its exotic Ditizaian Wolf Cutlet and Its Exotic Monkey Cutlet.\n"
;;      "The planet Ditiza is reasonably fabled for Ditizian evil juice and its inhabitants' ingrained silliness."]
;;     [true
;;      false
;;      "ANLE"
;;      "The world anle is mildly fabled for its inhabitants' ancient loathing of discos and its unusual Its Fabulous Goat Soup.\n"
;;      "The world Anle is notable for its great tropical forests and Anlian evil brandy."]
;;     [true
;;      true
;;      "ONISQU"
;;      "This planet is a dull place.\n"
;;      "This planet is a dull place."]
;;     [true
;;      false
;;      "ALEUSQU"
;;      "This world is reasonably notable for its great oceans but ravaged by a deadly disease.\n"
;;      "This world is reasonably notable for its great volcanoes but ravaged by vicious vicious shrews."]
;;     [true
;;      true
;;      "ZASOCEAT"
;;      "Zasoceat is a revolting dump.\n"
;;      "Zasoceat is a revolting dump."]
;;     [true
;;      true
;;      "RILACE"
;;      "The world rilace is a dull world.\n"
;;      "The world Rilace is a dull world."]
;;     [true
;;      false
;;      "BEENRI"
;;      "This planet is mildly noted for the beenriian edible wasp but plagued by evil tree leopards.\n"
;;      "This planet is mildly noted for the Beenrian mountain Esseina?oid but scourged by frequent civil war."]
;;     [true
;;      false
;;      "LAEDEN"
;;      "The planet laeden is very noted for its hoopy night life and Laedenian Ultra Hockey.\n"
;;      "The planet Laeden is reasonably fabled for its exciting sit coms and its inhabitants' exceptional love for food blenders."]
;;     [true
;;      false
;;      "MARIAR"
;;      "This world is  fabled for its strange mountains.\n"
;;      "This world is fabled for its unusual tropical forests."]
;;     [true
;;      true
;;      "RIISER"
;;      "Riiser is cursed by dreadful civil war.\n"
;;      "Riiser is cursed by dreadful civil war."]
;;     [true
;;      false
;;      "QUTIRI"
;;      "The world qutiri is mildly noted for its unusual mountains but beset by unpredictable solar activity.\n"
;;      "The world Qutiri is mildly noted for its ancient mountains but plagued by deadly earthquakes."]
;;     [true
;;      true
;;      "BIRAMABI"
;;      "The world biramabi is a dull world.\n"
;;      "The world Biramabi is a dull world."]
;;     [true
;;      true
;;      "SOORBI"
;;      "The planet soorbi is an unremarkable dump.\n"
;;      "The planet Soorbi is an unremarkable dump."]
;;     [true
;;      false
;;      "SOLAGEON"
;;      "This world is most famous for its vast rock formations and its vast mountains.\n"
;;      "This world is very well known for Solageonian lethal water and the Solageonian tree wolf."]
;;     [true
;;      false
;;      "TIQUAT"
;;      "This world is reasonably notable for its great oceans but ravaged by a evil disease.\n"
;;      "This world is reasonably well known for its great parking meters but cursed by unpredictable earthquakes."]
;;     [true
;;      false
;;      "REXEBE"
;;      "This world is reasonably noted for its inhabitants' exceptional love for food blenders and the rexebeian killer monkey.\n"
;;      "This world is mildly famous for its hoopy night life and its exotic cuisine."]
;;     [true
;;      true
;;      "QUBEEN"
;;      "This world is ravaged by unpredictable civil war.\n"
;;      "This world is ravaged by unpredictable civil war."]
;;     [true
;;      false
;;      "CETIISQU"
;;      "This planet is reasonably famous for the cetiisquian evil araboid.\n"
;;      "This planet is reasonably famous for the Cetiisqian evil Stoid."]
;;     [true
;;      false
;;      "REBIA"
;;      "Rebia is very notable for its inhabitants' funny silliness.\n"
;;      "Rebia is very notable for its inhabitants' weird shyness."]
;;     [true
;;      false
;;      "ORDIMA"
;;      "This planet is reasonably noted for Its Fabulous Goat Soup.\n"
;;      "This planet is reasonably noted for its exotic goat soup."]
;;     [true
;;      false
;;      "ARUSZATI"
;;      "This planet is reasonably noted for Zero-g cricket.\n"
;;      "This planet is noted for Zero-G cricket."]
;;     [true
;;      true
;;      "ZALERIZA"
;;      "This world is a tedious place.\n"
;;      "This world is a tedious place."]
;;     [true
;;      true
;;      "ZASOER"
;;      "Zasoer is mildly well known for its exotic night life.\n"
;;      "Zasoer is mildly well known for its exotic night life."]
;;     [true
;;      false
;;      "RALEEN"
;;      "This planet is mildly notable for its inhabitants' eccentric silliness and its exciting sit coms.\n"
;;      "This planet is notable for the Raleenian tree grub and its inhabitants' unusual silliness."]
;;     [true
;;      false
;;      "QURAVE"
;;      "The planet qurave is mildly notable for Quraveian Killer Water.\n"
;;      "The planet Qurave is mildly notable for Quravian Zaaronen brandy."]
;;     [true
;;      false
;;      "ATREBIBI"
;;      "The world atrebibi is most famous for the atrebibiian spotted cat.\n"
;;      "The world Atrebibi is most famous for the Atrebibian deadly monkey."]
;;     [true
;;      false
;;      "TEESDI"
;;      "Teesdi is  famous for Teesdiian Shrew Brew but plagued by vicious killer beasts.\n"
;;      "Teesdi is famous for Teesdian shrew cutlet but ravaged by occasional solar activity."]
;;     [true
;;      false
;;      "ARARUS"
;;      "Ararus is reasonably fabled for its fabulous cuisine and its inhabitants' ancient shyness.\n"
;;      "Ararus is most famous for its pink Esoneril tulip plantations and its weird exuberant forests."]
;;     [true
;;      true
;;      "ARA"
;;      "The world ara is scourged by a evil disease.\n"
;;      "The world Ara is scourged by a evil disease."]
;;     [true
;;      false
;;      "TIANVE"
;;      "The planet tianve is  well known for the tianveian vicious snail and Zero-g hockey.\n"
;;      "The planet Tianve is reasonably noted for its inhabitants' exceptional loathing of food blenders and Zero-G cricket."]
;;     [true
;;      false
;;      "QUORTE"
;;      "Quorte is  well known for the quorteian vicious  but cursed by occasional earthquakes.\n"
;;      "Quorte is well known for the Quortian tree wolf but scourged by dreadful solar activity."]
;;     [true
;;      false
;;      "SOLADIES"
;;      "This planet is  fabled for its exciting Soladiesian Deadly Water.\n"
;;      "This planet is fabled for its exciting Soladiesian evil brandy."]
;;     [true
;;      false
;;      "MAXEEDSO"
;;      "This world is reasonably fabled for Mud Polo and the maxeedsoian edible ant.\n"
;;      "This world is mildly famous for its vast rain forests and the Maxeedsian tree wolf."]
;;     [true
;;      true
;;      "XEXEDI"
;;      "The planet xexedi is scourged by a deadly disease.\n"
;;      "The planet Xexedi is scourged by a deadly disease."]
;;     [true
;;      false
;;      "XEXETI"
;;      "This planet is mildly notable for its inhabitants' exceptional shyness and its exciting casinos.\n"
;;      "This planet is notable for the Xexetian edible arts graduate and its great volcanoes."]
;;     [true
;;      false
;;      "TIINLEBI"
;;      "The planet tiinlebi is  notable for the tiinlebiian spotted lobstoid and its inhabitants' ingrained silliness.\n"
;;      "The planet Tiinlebi is most noted for the Tiinlebian mountain slug and its inhabitants' exceptional loathing of food blenders."]
;;     [true
;;      true
;;      "RATEEDAR"
;;      "Rateedar is cursed by dreadful civil war.\n"
;;      "Rateedar is cursed by dreadful civil war."]
;;     [true
;;      true
;;      "ONLEMA"
;;      "This world is plagued by frequent solar activity.\n"
;;      "This world is plagued by frequent solar activity."]
;;     [true
;;      true
;;      "ORERVE"
;;      "This planet is a dull place.\n"
;;      "This planet is a dull place."])
