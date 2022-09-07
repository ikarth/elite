(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   [datascript.core :as d]
   [clojure.spec.alpha :as spec]
   ;;[clojure.edn :as edn]
   [clojure.string :as cstring]
   [clojure.math]
   ;;[grotesque.core :as grot]
   ;;["js-xxhash" :as xx :refer (xxHash32)]

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log-db
  "Log a complete listing of the entities in the provided `db` to the console."
  [db]
  (d/q '[:find ?any ?obj :where [?obj :type ?any]] @db)) ;todo: log to console...




(defn positions
  "https://stackoverflow.com/a/4831131/5562922"
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

(defn fetch-internal-view
  "Returns the internal database. Intended mostly for debugging visualization."
  []
  (if-let [db-conn (get @current-database :db-conn)]
    (vec (map (fn [dat]
                (let [[e a v tx add] dat]
                  [e a v tx add])) (d/datoms @db-conn :eavt)))
    (println "Database connection missing when trying to fetch a new view.")))

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


;; (def elite-schema {:seed/planet            {:db/cardinality :db.cardinality/one   :db/unique :db.unique/identity}
;;                    :seed/description       {:db/cardinality :db.cardinality/one}
;;                    :seed/galaxy            {:db/cardinality :db.cardinality/one}
;;                    :planet/economy-type    {:db/cardinality :db.cardinality/one}
;;                    :planet/species         {:db/cardinality :db.cardinality/one}
;;                    :planet/government-type {:db/cardinality :db.cardinality/one}
;;                    :planet/name-length     {:db/cardinality :db.cardinality/one}
;;                    :planet/partial-name    {:db/cardinality :db.cardinality/one}
;;                    :planet/name            {:db/cardinality :db.cardinality/one}
;;                    :planet/description     {:db/cardinality :db.cardinality/one}}
;;   )







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








;; (spec/def :seed/data
;;   (spec/and
   
;;    )
;;   )

(def elite-index
  {:s0_lo 0 ;; QQ15
   :s0_hi 1 ;; QQ15+1
   :s1_lo 2 ;; QQ15+2
   :s1_hi 3 ;; QQ15+3
   :s2_lo 4 ;; QQ15+4
   :s2_hi 5 ;; QQ15+5
  })

(defn get-seed-bytes [seed]
  ;;{:pre [(spec/valid? :seed/data seed)]}
  (into [] (map #(. seed getUint8 %) (range 6) )))


(defn make-seed [seed-vals]
  {:pre  [(spec/valid? :seed/specifier seed-vals)]
   ;;:post [(spec/valid? :seed/data-array %)]
   }
  (let [ab (js/ArrayBuffer. 6)
      view (js/DataView. ab)]
  (. view setUint16 0 (nth seed-vals 0) true)    
  (. view setUint16 2 (nth seed-vals 1) true)    
  (. view setUint16 4 (nth seed-vals 2) true)
  ;;(aset view "toStringTag" (get-seed-bytes view))
  view
  ))



(defn bytes-to-seed [seed-bytes]
  (let [ab (js/ArrayBuffer. 6)
        view (js/DataView. ab)]
    (doseq [n (range 6)]
      (. view setUint8 n (nth seed-bytes n) true))
    ;;(aset view "toStringTag" (get-seed-bytes view))
    view
    ))

(bytes-to-seed [0 1 2 3 4 5])
(println (bytes-to-seed [0 1 2 3 4 5]))
(get-seed-bytes (bytes-to-seed [0 10 20 30 40 50]))



(defn byte-to-bin [dec]
  (let [byte-length 8
        num-vec (vec (. (bit-shift-right dec 0) toString 2))
        extra (- byte-length (count num-vec))]
    (map #(js/parseInt % 2)
         (concat (take extra (repeat 0))
                 num-vec))))

(defn bin-to-byte
  "Convert a vector of 0s and 1s to a number."
  [bin]
  (reduce +
          (map #(apply * %)
               (map vector bin 
                    (rseq (into [] (map #(Math/pow 2 %) (range (count bin)))))))))

;;(= (bin-to-byte [1 1 0 1 1 1 1 1]) 223)

;;(map bin-to-byte(map byte-to-bin (get-seed-bytes (make-seed [9991494 14 98]))))

(defn get-seed-bits [seed byte-index start-index count-index]
  (subvec (into [] (nth (map byte-to-bin (get-seed-bytes seed)) byte-index))
          start-index
          (+ start-index count-index)))

(defn get-seed-byte-8 [seed byte-index]
  (. seed getUint8 byte-index))

(defn get-seed-byte-16 [seed byte-index]
  (. seed getUint16 byte-index))


(defn get-value-from-seed [seed byte-index start-index count-index]
  (let [array-of-bits (get-seed-bits seed byte-index start-index count-index)]
    (js/parseInt (cstring/join "" array-of-bits))))




(defn twist-seed
  "Takes a random seed and twists it using Elite's 'tribonocci' method."
  [old-seed]
  (let [bytes (get-seed-bytes old-seed)
        twisted [(+ (nth bytes 2)
                    (* 256 (nth bytes 3)))
                 (+ (nth bytes 4)
                    (* 256 (nth bytes 5)))
                 (mod (+ (+ (nth bytes 0) (nth bytes 2) (nth bytes 4))
                         (* 256 (+ (nth bytes 1) (nth bytes 3) (nth bytes 5)))
                         )
                      65536)]]
    ;;(map )(map byte-to-bin twisted)
    (make-seed twisted)))


(defn hyperjump [old-seed]
  (let [bits (map byte-to-bin (map #(. old-seed getUint8 %) (range 6)))
        after-jump (map (fn [seed]
                          (concat (rest seed) [(first seed)]))
                        bits)
        ]
    ;; (println bits)
    ;; (println after-jump)
    (bytes-to-seed(map bin-to-byte after-jump))))


;; Tests for making and twisting seeds
(= "4a5a48253b7"
   (apply
    str
    (map #(. % toString 16)
         (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753])))))


(= '("48" "2" "53" "b7" "e5" "13")
   (map #(. % toString 16)
        (get-seed-bytes
         (twist-seed
          (make-seed [0x5A4A 0x0248 0xB753])))))

(= '("53" "b7" "e5" "13" "80" "cd")
   (map #(. % toString 16)
        (get-seed-bytes
         (twist-seed
          (twist-seed
           (make-seed [0x5A4A 0x0248 0xB753]))))))

(= '("80" "cd" "b8" "98" "1d" "7a")
   (map #(. % toString 16)
        (get-seed-bytes
         (twist-seed
          (twist-seed
           (twist-seed
            (twist-seed
             (make-seed [0x5A4A 0x0248 0xB753]))))))))


(= [128 205 184 152 29 122]
 (get-seed-bytes
  (twist-seed
   (twist-seed
    (twist-seed
     (twist-seed
      (make-seed [0x5A4A 0x0248 0xB753])))))))

;; Test making seeds from input
(= [74 90 72 2 83 183] (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753])))
(= '((0 1 0 0 1 0 1 0)
     (0 1 0 1 1 0 1 0)
     (0 1 0 0 1 0 0 0)
     (0 0 0 0 0 0 1 0)
     (0 1 0 1 0 0 1 1)
     (1 0 1 1 0 1 1 1))
   (map byte-to-bin (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753]))))

;; Test hyperjump
(= '("94" "b4" "90" "4" "a6" "6f")
 (map #(. % toString 16)
      (get-seed-bytes
       (hyperjump (make-seed [0x5A4A 0x0248 0xB753])))))






(defn twist-to-next-planet [planet-seed]
  (-> planet-seed
      twist-seed
      twist-seed
      twist-seed
      twist-seed))


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
  ;; (println "extract-seed-for-name-length")
  ;; (println (get-value-from-seed seed (:s0_lo elite-index) 0 8))
  ;; (println (map byte-to-bin (get-seed-bytes seed)))
  ;; (println (get-value-from-seed seed (:s0_lo elite-index) 1 1))
  ;;(println (get-value-from-seed seed (:s0_hi elite-index) 0 8))
  (if (= 1 (get-value-from-seed seed (:s0_lo elite-index) 1 1))
    4
    3))


;; (extract-seed-for-name-length (make-seed [65035 14 98]))
;; =======
;;     4 3))

;; Test (bytes)
(= [131 234 135 25 255 255]
   (get-seed-bytes
    (make-seed [60035 6535 65535])))

(defn generate-name-start [seed]
  (let [token-seed seed
        name-length-remaining (extract-seed-for-name-length seed)
        planet-name ""
        ]
    ;;(println name-length-remaining)
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


;;(exec-func db-conn )


(defn generate-name
  "Generate the planet name recursively, using the elite-planet-name-diagraphs table as the source data.
  
  Having this operation be recursive follows the original BBC Elite, plus it lets us demonstrate how the generator can handle partially-constructed artifacts."
  [input]
  (if (string? input)
    input
    (let [[seed-token name-length-remaining name-in-progress] input]
      (if (< name-length-remaining 1)
        name-in-progress
        (let [new-token (twist-seed seed-token)
              index-bits (get-seed-bits seed-token
                                        (:s2_hi elite-index) 3 5)
              digraph-index (bin-to-byte index-bits)
              digraph (get elite-planet-name-digraphs digraph-index)
              ]
          ;; (println [(get-seed-bits seed-token
          ;;                          (:s2_hi elite-index) 0 8)
          ;;           index-bits
          ;;           digraph
          ;;           digraph-index
          ;;           name-length-remaining])
          [new-token
           (dec name-length-remaining)
           (str name-in-progress digraph)
           ]
          )))))

 

(defn planet-government
  "Planet government is a number from 0 to 7, extracted directly from the bits in the seed.

  The first operation in the original code, and the most basic."  
  [planet-seed]  
  (bin-to-byte
   (get-seed-bits planet-seed 
                  (:s1_lo elite-index)
                  2 3)))

(defn invert-bits
  "Invert the bits in a boolean vector.
  There's probably a built-in way to do this more succinctly that I'm forgetting."
  [bits]
  (map #(if (= % 0) 1 0) bits))

(defn left-trim
  "Trim leading zeros from a collection."
  [col]
  (subvec (into [] col) 
          (first (positions #{1} col))))


(defn invert-byte
  "Given a number, break it down to its bit representation, invert the bits, and return it as a number."
  [number-byte size]

  (bin-to-byte
   (invert-bits
    (subvec (into [] (byte-to-bin number-byte)) (- 8 size) 8))))

;; (byte-to-bin 5)
;; (invert-byte 5 3)

;; (subvec
;;  (into [] (invert-byte 5 3))
;;  0 3
;;  )

(defn planet-economy
  "Generate the economic level and type of the planet.
  Note that we need the governement type, because anarchy and feudal governements are restricted from being rich.
  We *could* just calculate the government type on the fly, since in the original algorithm this was all one dense code block that fed into the next step, but for the purposes of our more flexible generator, I'm opting to make it a parameter - maybe there's some new operation that alters the government type or something, so we'll want the final value instead of a parallel calculation. But in other circumstances we might opt for the parallel calculation."
  [planet-seed planet-government]
  (let [eco-base
        (try 
          (get-seed-bits planet-seed
                         (:s0_hi elite-index)
                         5 3)
          (catch js/Error e
            (println "An economic errort occurred:" e)
              0))
        adjusted (assoc eco-base 1 (if (< planet-government 2) 1 (nth eco-base 1)))
        type (nth eco-base 0)
        prosperity (try
                     (bin-to-byte adjusted)
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
  (+ (invert-byte (second economy) 3)
     (bin-to-byte
      (get-seed-bits planet-seed (:s1_hi elite-index) 6 2))
     (clojure.math/ceil (/ government 2))))


(defn planet-tech-level-from-prosperity
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)

  There are two of these functions because the ecological query was
  easier to write if it used prosperity directly." 
  [planet-seed prosperity government]
  (+ (invert-byte prosperity 3)
     (bin-to-byte
      (get-seed-bits planet-seed
                     (:s1_hi elite-index)
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

(defn bitwise-add-vec
 "take two boolean vectors and add them, emulating the BBC micro behavior"
 [one two]
 (byte-to-bin
  (+
   (bin-to-byte one)
   (bin-to-byte two))))

;; 01234567
;; 76543210

(defn planet-species
  "Generate a (brief) description of the local planetary species."
  [seed]
   ;; Check it 7 of s2_lo - if it is zero return "Human Colonials"
  (if (= 0 
         (first (get-seed-bits seed (:s2_lo elite-index) 0 1))
         )
    "Human Colonials"
    (let [;; A bunch of fiddly bitmapping operations. Remember that our subvec indexing is counting from the opposite direction as the notes on the BBC Elite site...
          ;; register-A = s2_hi
          register-A 
          (vec (concat [0 0]
                       (into [] (get-seed-bits seed (:s2_hi elite-index) 0 6))))
          ;; bits 2-4 of A, by using a mask which multiplies the bit strings together 
          species-size  (bin-to-byte (mapv * [0 0 0 0 0 1 1 1] register-A))
          ;; bits 5-7 of A, by just grabbing the subvec 
          species-color (subvec register-A 3 5)
          ;; A = bits 0-2 of (s0_hi EOR s1_hi)
          register-A-new
          (mapv (fn [a b] (if (not= a b) 1 0))
                (get-seed-bits seed (:s0_hi elite-index) 0 8)
                (get-seed-bits seed (:s1_hi elite-index) 0 8))
          ;; texture = bits 0-2 of the new A
          texture (mapv * [0 0 0 0 0 1 1 1]
                        register-A-new)
          ;; bits 0-1 of s2_hi
          intermediate-B (mapv * [0 0 0 0 0 0 1 1]
                           (get-seed-bits seed (:s2_hi elite-index) 0 8))
          ;; add register B to A-new
          intermediate-B-plus (bitwise-add-vec texture
                                           (mapv * [0 0 0 0 0 0 1 1]
                                                 intermediate-B))
          ;; take bits 0-2 of B-plus
          species-name (mapv * [0 0 0 0 0 1 1 1]
                             intermediate-B-plus)         
          type (bin-to-byte (get-seed-bits seed (:s2_hi elite-index) 6 2))
          species-id [(bin-to-byte (get-seed-bits seed (:s2_hi elite-index) 3 3)) ;; size
                      (bin-to-byte (get-seed-bits seed (:s2_hi elite-index) 0 3)) ;; color
                      (bin-to-byte (subvec texture 5)) ;; texture                  
                      (bin-to-byte species-name)]] ;; type
      (apply str 
       (map #(get %2 %1) species-id species-table)))))

;; Test generating species descriptions and planet names...
(comment
  (planet-species (make-seed [0x57fa 0x1d30 0x17b3]))
  (planet-species (make-seed [0x588a 0x476c 0x02db]))
  (last (take 6
              (iterate
               generate-name (generate-name-start (make-seed [0xfa57 0x301d 0xb317])))))
  (last (take 6
              (iterate
               generate-name (generate-name-start (make-seed [0x57fa 0x1d30 0x17b3]))))))


(defn planet-population-size [tech-level economy government]
  ;;(println tech-level economy government)
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (seq? economy)
          (second economy)
          economy)]
  (+
   (* tech-level 4)
   econ-prosperity
   government
   1)))

(defn planet-productivity [economy government population]
  ;;(println "planet-prodcutivity")
  ;;(println economy government population)
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (seq? economy)
          (second economy)
          economy)]
  (* 8
     (+ (invert-byte econ-prosperity 3) 3)
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

;; The seed for the orignal game, chosen after searching through many possibilities...
(def elite-seed (make-seed [0x5A4A 0x0248 0xB753]))

;; test twisting the seed
(def planet-two (-> elite-seed
                    twist-seed
                    twist-seed
                    twist-seed
                    twist-seed))

;; test getting all of the planets
(def planet-seed-list
  (reduce (fn [current next-id] (concat current [(twist-to-next-planet (last current))]))
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

(defn test-galaxy-generator []
  (let []
    (println "\n\n\n\n\n\n\n\n\n\n\n\n")
    (doseq [[r p] (map-indexed (fn [index item]
                                 [index item])
                               planet-seed-list)
            ]
      
      (let [gov (planet-government p)
            econ (planet-economy p (planet-government p))
            tech (planet-tech-level p econ gov)
            pop (planet-population-size tech econ gov)
            prod (planet-productivity econ gov pop)
            name (last (take 7 (iterate generate-name (generate-name-start p))))
            species (planet-species p)
            ]
        (if true;(< 245 r )
          (println 
           (map 
            #(str %1 ":\t "%2 "\n")
            ["id" "name" "seed" "species" "government" "economy" "tech-level" "population size" "productivity"]
            [r
             name
             (map #(. % toString 16) (get-seed-bytes p))
             species
             (government-name gov)
             (economy-name econ)
             tech
             pop
             prod
             ])))))))

(test-galaxy-generator )




;; (def generative-operations
;;   [;;{:op-function generate-galaxy}
;;    {:op-function make-seed}
;;    ;;{:op-function next-planet}
;;    {:input :seed/planet-seed
;;     :output :planet/government-type 
;;     :op-function planet-government}
;;    {:op-function planet-economy}
;;    {:op-function planet-tech-level}
;;    {:op-function planet-population-size}
;;    {:op-function planet-productivity}
   
;;    {:input :seed/planet-seed
;;     :output [:seed/name-token-seed
;;              :number/planet-name-length
;;              :string/planet-name-partial]    
;;     :op-function generate-name-start
;;     {:input [:seed/name-token-seed
;;              :number/planet-name-length
;;              :string/planet-name-partial]
;;      :output []
;;      :op-function generate-name

;;      }}
;;    {:op-function planet-species}
;;    ]
;;   )







(defn is-seed? [possible-seed]
  true ;; TODO: actually check
  )

{:input
 {:int-seed/planet-seed #(and integer?
                              is-seed?)}
 :output
 {:int-parameter/name-length
  (fn [n] (and integer?
               (< 2 n 5)))}}



(spec/def ::xplanet-seed
  (fn [n]
    (and (integer? n)
         
         )))

(spec/valid? ::xplanet-seed 866)


(egrammar/goat-soup 0 "Santa Cruz")


(defn run []
  (.log js/console "Hello")
  (println "World"))





;; (def elite-db-conn (d/create-conn elite-schema))
;; (swap! current-database assoc-in [:db-conn]
;;        elite-db-conn)
;; (swap! current-database assoc-in [:db-schema]
;;        elite-schema)

;; (d/transact! elite-db-conn [{:test/name "Name of Test"}])
;; ;(log-db elite-db-conn)
;; (d/transact! elite-db-conn [{:seed/galaxy (make-seed [0x5A4A 0x0248 0xB753])}])
;; (d/q '[:find ?e ?v
;;        :where
;;        [?e :seed/planet-seed ?v]]
;;      @elite-db-conn)

(defn make-planet
  "Returns the seed for the planet at planet-index."
  [galaxy-seed galaxy-index planet-index]
  [{:db/id -1
    :planet/galaxy galaxy-index
    :planet/index planet-index
    :seed/planet
    (if (> 0 planet-index)
      (last (take planet-index (iterate twist-to-next-planet galaxy-seed)))
      galaxy-seed)}])

(make-planet [0x5A4A 0x0248 0xB753] 0 62)


;; (make-seed [0x5A4A 0x0248 0xB753])


;; (let [galaxy-seed (d/q '[:find ?gal-seed
;;                         ;;:in $ %
;;                         :where [?e :seed/galaxy ?gal-seed]]
;;                        @elite-db-conn)
;;       planet-index-number 7
       
;;       parameters [galaxy-seed planet-index-number]]
;;   (d/transact! elite-db-conn
;;                (make-planet galaxy-seed planet-index-number))

;;   )


;; (d/q '[:find ?e ?v
;;        :where
;;        [?e :seed/planet-seed ?v]]
;;      @elite-db-conn)

;; (d/q '[:find ?e ?v
;;        :where
;;        [3  ?v]]
;;      @elite-db-conn)

