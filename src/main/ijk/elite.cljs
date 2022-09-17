(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   [ijk.elite-utility :as utility]
   [datascript.core :as d]
   [clojure.spec.alpha :as spec]
   ;;[clojure.edn :as edn]
   [clojure.string :as cstring]
   [clojure.math]
   [clojure.math.combinatorics :as combo]
   [cljs.pprint :as pprint]
   [clojure.set :refer [union]]
   ;;["js-xxhash" :as xx :refer (xxHash32)]
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The seed for the orignal game, chosen after searching through many possibilities...
(def elite-seed (utility/make-seed [0x5A4A 0x0248 0xB753]))

;; test twisting the seed
(def planet-two (-> elite-seed
                    utility/twist-seed
                    utility/twist-seed
                    utility/twist-seed
                    utility/twist-seed))

;; test getting all of the planets
(defn planet-seed-list
  ([] (planet-seed-list elite-seed))
  ([seed]
   (reduce (fn [current next-id] (concat current [(utility/twist-to-next-planet (last current))]))
           [seed]
           (range 255))))


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


;; .QQ16 in the original code.
;; I'm storing them from 0-31 here instead of the original 128 to 159,
;; because on a modern system I don't need to cram it into the same memory
;; as the other text glyphs and commands so we save an extra step.
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
   ])


(defn extract-bits
  "Utility function to streamline extracting specific parts of the seed.
  Takes seed, the byte, the starting bit, and how many total bits."
  [seed word start amount]
  (utility/bin-to-byte (utility/get-seed-bits seed (word utility/elite-index) start amount)))


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
  
  Having this operation be recursive across multiple calls follows the original BBC Elite, plus it lets us demonstrate how the generator can handle partially-constructed artifacts."
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
           ])))))

 

(defn galactic-coordinates [seed]
  (let [galactic-x (extract-bits seed :s1_hi 0 7)
        galactic-y (/ (extract-bits seed :s0_hi 0 7) 2)
        size-on-map (utility/bin-to-byte (mapv max [0 1 0 1 0 0 0 0] 
                                         (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 7)))]
    {:planet/galactic-x  galactic-x
     :planet/galactic-y  galactic-y
     :planet/size-on-map size-on-map}))

(defn galactic-x [seed]
  (extract-bits seed :s1_hi 0 8))

(defn galactic-y [seed]
  (extract-bits seed :s0_hi 0 8))

(defn size-on-map [seed]
  (utility/bin-to-byte (mapv max [0 1 0 1 0 0 0 0] (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 8))))

(defn planet-government
  "Planet government is a number from 0 to 7, extracted directly from the bits in the seed.

  The first operation in the original code, and the most basic."  
  [planet-seed]
  (extract-bits planet-seed :s1_lo 2 3))


(defn planet-economy
  "Generate the economic level and type of the planet.
  Note that we need the governement type, because anarchy and feudal governements are restricted from being rich.
  We *could* just calculate the government type on the fly, since in the original algorithm this was all one dense code block that fed into the next step, but for the purposes of our more flexible generator, I'm opting to make it a parameter - maybe there's some new operation that alters the government type or something, so we'll want the final value instead of a parallel calculation. But in other circumstances we might opt for the parallel calculation."
  [planet-seed planet-government]
  (let [eco-base
        (try 
          (utility/get-seed-bits planet-seed (:s0_hi utility/elite-index) 5 3)
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
     (extract-bits planet-seed :s1_hi 6 2)
     (clojure.math/ceil (/ government 2))))


(defn planet-tech-level-from-prosperity
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)

  There are two of these functions because the ecological query was
  easier to write if it used prosperity directly." 
  [planet-seed prosperity government]
  (+ (utility/invert-byte prosperity 3)
     (extract-bits planet-seed :s1_hi 6 2)
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
  (egrammar/generate-goat-soup seed system-name))

(defn planet-species
  "Generate a (brief) description of the local planetary species."
  [seed]
   ;; Check bit 7 of s2_lo - if it is zero return "Human Colonials"
  (if (= 0 
         (first (utility/get-seed-bits seed (:s2_lo utility/elite-index) 0 1)))
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

(defn planet-productivity [economy government population]
  (let [econ-prosperity ;; econ can be passed in as a tuple of [type, prosperity]
        (if (or (vector? economy)(seq? economy))
          (second economy)
          economy)]
  (* 8
     (+ (utility/invert-byte econ-prosperity 3) 3)
     (+ government 4)
     population)))

(defn government-name [gov-type]
  (nth [:anarchy :feudal :multi-government :dictatorship :communist :confederacy :democracy :corporate-state]
       gov-type
       :unknown))

(defn economy-name [[type prosperity]]
  [(get {0 :rich
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
  (count (list-reachable-systems system-coordinates list-of-planet-coordinates jump-range)))







(defn round-pop [pop]
  (/ (.round js/Math (* 10 pop)) 10))

(defn test-galaxy-generator []
  (let []
    (println "\n\n\n\n\n\n\n\n\n\n\n\n")
    (doseq [[r p] (map-indexed (fn [index item]
                                 [index item])
                               (planet-seed-list))]      
      (let [planet-coord-list (map-indexed (fn [index p] [index [(galactic-x p) (galactic-y p)]]) (planet-seed-list))
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

;; (map-indexed (fn [index p]
;;                [index [(galactic-x p) (galactic-y p)]]) planet-seed-list)

;; (mapv planet-government planet-seed-list)
;; (mapv #(planet-economy %1 (planet-government %1)) planet-seed-list)


;; (let [planet-coord-list (mapv)])


;;  (nth planet-seed-list 7)


;; (planet-government (nth planet-seed-list 7))
;; (planet-economy (nth planet-seed-list 7) 3)

(defn export-galaxy [galaxy-seed]
  (let [jump-range 70
        this-galaxy-planet-list (planet-seed-list galaxy-seed)
        indexed-planets
        (map-indexed (fn [i p] [i p]) this-galaxy-planet-list)
        planet-coord-list (map-indexed (fn [index p] [index [(galactic-x p) (galactic-y p)]]) (planet-seed-list galaxy-seed))
        planets-in-galaxy        
        (for [[index p] indexed-planets
              :let [name (last (take 7 (iterate generate-name (generate-name-start p))))
                    gov  (planet-government p)
                    econ (planet-economy p gov)
                    tech (planet-tech-level p econ gov)
                    pop  (planet-population-size tech econ gov)
                    prod (planet-productivity econ gov pop)
                    galactic-x (galactic-x p)
                    galactic-y (galactic-y p)
                    reachable-systems (list-reachable-systems [galactic-x galactic-y] planet-coord-list jump-range)
                    hub-count  (count reachable-systems)
                    description (planet-goat-soup p name)
                    ]]
          {:index      index
           :name       name     
           :species    (planet-species p)
           :government (government-name gov)
           :economy    (economy-name econ)
           :tech-level tech
           :population pop
           :productivity prod
           :location   [galactic-x galactic-y]
           :neighbors  reachable-systems
           :neighbor-count hub-count
           :description description     
           })]
    planets-in-galaxy))

(defn export-planet-names [galaxy-seed]
  (let [this-galaxy-planet-list (planet-seed-list galaxy-seed)]
    (map (fn [p]
           (last (take 7 (iterate generate-name (generate-name-start p)))))
         this-galaxy-planet-list)))

(count
 (set
  (export-planet-names elite-seed)))

(export-planet-names elite-seed)


(clj->js (first (export-galaxy elite-seed)))

(comment
  (utility/write-to-file
   (export-galaxy elite-seed)
   "elite-galaxy-01.edn")
  (utility/write-to-file
   (.stringify js/JSON
               (clj->js (export-galaxy elite-seed)))
   "elite-galaxy-01.json"))


(def local-galaxy-seeds
  (take 8
        (iterate utility/galaxy-twist elite-seed)))

;; (mapv utility/get-seed-bytes
;;   local-galaxy-seeds)

(comment
  (for [ [i g] (map-indexed (fn [a b] [a b])
                            local-galaxy-seeds)]
    (let [gal-data (export-galaxy g)]
      (println "Galaxy #" i " : " (utility/get-seed-bytes g))
      (utility/write-to-file gal-data (str "elite-galaxy-0" i ".edn"))
      (utility/write-to-file (.stringify js/JSON (clj->js gal-data)) (str "elite-galaxy-0" i ".json"))
      )))
      
;; (count
;;  (set
;;   (export-planet-names elite-seed)))

;;(range 65535)

;;(make-seed )

;; (union (set [3 4]) (set [3 6]))

;; (combo/count-permutations [ 1 2 3])

;; (range 10)
(comment
  (let [galaxies-count 1
        planet-names-list
        (take (* galaxies-count 256)
              (let [galaxies            
                    (map utility/make-seed (take 15000000000000000 (combo/cartesian-product (range 65536) (range 65536) (range 65536))))]
                (map #(str (utility/get-seed-bytes %)) galaxies);;(map export-planet-names galaxies)        
                ))
        names-set (union (map set planet-names-list))
        names-count (count names-set)
        ]
    (utility/write-to-file (.stringify js/JSON (clj->js planet-names-list)) "planet-seeds.txt")
    
    (utility/write-to-file (str names-count " / " (* 256 galaxies-count) "\n" planet-names-list) "planet-names-count.txt")
    ))

(defn seed-as-int [seed]
  (apply + 
         (map * (map (fn [n] (reduce * (repeat n 256))) (range))
              (reverse
               (utility/get-seed-bytes seed))))) 

(defn write-random-galaxy []
  (let [galaxy-seed-numbers [(rand-int 65536) (rand-int 65536) (rand-int 65536)]
        galaxy-seeds (take 8 (iterate utility/galaxy-twist (utility/make-seed galaxy-seed-numbers)))
        galaxy-data (map export-galaxy galaxy-seeds)
        catalog (map (fn [data seeds index] [(str "Galaxy #" index " : " (seed-as-int seeds) " : " (utility/get-seed-bytes seeds)) data]) galaxy-data galaxy-seeds (range))
        ]
    (utility/write-to-file (.stringify js/JSON (clj->js catalog)) (str "galaxy_" (seed-as-int (first galaxy-seeds)) ".json"))
    "Wrote Galaxy To Disk"))

;; ()
;; ;;(union #{5 6} #{ 5 7})
;; (take 8 (map #(* 256 (* % %)) (range)))
;; (take 8 (map (fn [n] (reduce * (repeat n 256))) (range)))

;; (apply + [7 0 8])



;;; 
;; (apply concat (take 3 [[1 2 3] [4 5] [6 7]]))

;; => ((""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      ""
;;      "")
;;     (""
;;      ""
;;      "LE"
;;      "GEBIIN"
;;      "ESININLE"
;;      "QUUSZA"
;;      "BEONQUDI"
;;      "SOAZAAN"
;;      "ERERTEIS"
;;      "ARZADI"
;;      "REEDGE"
;;      "SOVEESSO"
;;      "USORES"
;;      "DILEORES"
;;      "ZASOLAXE"
;;      "ONEDQUEN"
;;      "CEENAR"
;;      "RIARMARA"
;;      "ARESSO"
;;      "INATIRA"
;;      "ANSOTIQU"
;;      "ORMABI"
;;      "ENORENRI"
;;      "ESORLE"
;;      "ONCEUSDI"
;;      "ANATQUBI"
;;      "ENBEIN"
;;      "ENATTI"
;;      "ZAVEVE"
;;      "ONSOTI"
;;      "ISBEA"
;;      "GERAOR"
;;      "RIUSED"
;;      "LEORQU"
;;      "SOVEEN"
;;      "ORTESO"
;;      "LEBIBIA"
;;      "ISBE"
;;      "CETIDIAR"
;;      "ASOLEOR"
;;      "XEISOROR"
;;      "AXEMA"
;;      "ARENOR"
;;      "LEERUSAN"
;;      "RAEDSO"
;;      "RAERORON"
;;      "ARGEDIAN"
;;      "MARAANAN"
;;      "MAGEAR"
;;      "SOMADI"
;;      "ORSOLE"
;;      "ARREQUBE"
;;      "TEAROR"
;;      "ZAONZAUS"
;;      "INORDIEN"
;;      "ORORBI"
;;      "ORORDILE"
;;      "USVEON"
;;      "LALAAR"
;;      "RARARI"
;;      "EDISIS"
;;      "RERION"
;;      "MATIAAT"
;;      "TIEDXE"
;;      "DILELE"
;;      "ENXEZA"
;;      "ONTERA"
;;      "ONGEEN"
;;      "ARATATDI"
;;      "ERREIN"
;;      "BIRIATTI"
;;      "SOERREBI"
;;      "CEQUCEZA"
;;      "ZAAOR"
;;      "LATERI"
;;      "MAANTIRI"
;;      "ATBIVE"
;;      "MAERESCE"
;;      "ONARAGE"
;;      "SOINLA"
;;      "XEGEAN"
;;      "ISONTE"
;;      "ORRAAR"
;;      "EDTEDIXE"
;;      "INISINVE"
;;      "ONXEEDQU"
;;      "LAUSORTI"
;;      "ORARAN"
;;      "ZARILEZA"
;;      "CEMAVESO"
;;      "ESBITI"
;;      "BIBILA"
;;      "ISRABE"
;;      "DICETI"
;;      "USMARIEN"
;;      "ANINAN"
;;      "MAAROR"
;;      "BEANRI"
;;      "BIATCE"
;;      "LAORDI"
;;      "XEINMAAN"
;;      "BEQUUS"
;;      "EDVEZABE"
;;      "AUSMAGE"
;;      "TIBIXE"
;;      "ESINTI"
;;      "ATLARA"
;;      "CELAVEAT"
;;      "ISCERA"
;;      "ENONESAN"
;;      "CEMAINTE"
;;      "BETEDIRI"
;;      "USRAAN"
;;      "ESRILEES"
;;      "USENZA"
;;      "VEQUERLE"
;;      "DIONREAN"
;;      "ARLATETE"
;;      "AUSRAIN"
;;      "ESAR"
;;      "RIBEBIED"
;;      "ENATTEON"
;;      "INUSLA"
;;      "USUSTE"
;;      "ATANQU"
;;      "TEISON"
;;      "VERERIGE"
;;      "AATZA"
;;      "QUGEXE"
;;      "ZACEUS"
;;      "RITISO"
;;      "ANONED"
;;      "MATIVEED"
;;      "BILARA"
;;      "ORRIUS"
;;      "SOEREDER"
;;      "QUGEREIN"
;;      "RIORCE"
;;      "TEONOR"
;;      "AONCERA"
;;      "QURAGE"
;;      "ESRIORXE"
;;      "QUVEBECE"
;;      "AESRITI"
;;      "ONLAIN"
;;      "XEABECE"
;;      "USDI"
;;      "ZAUSGEAT"
;;      "ISENLEAT"
;;      "CEEDERA"
;;      "EDEDAT"
;;      "ESQULA"
;;      "ESEDQUTE"
;;      "ABIENSO"
;;      "QUXE"
;;      "QUQUBE"
;;      "VEATER"
;;      "ANZATI"
;;      "BEXEDIGE"
;;      "BECEIS"
;;      "EDMAQU"
;;      "CERIXE"
;;      "CEDIED"
;;      "ATLABE"
;;      "GEATERBI"
;;      "ESER"
;;      "MAVEANIS"
;;      "AUSLARE"
;;      "INERMAUS"
;;      "GETIGE"
;;      "EDORA"
;;      "ESORZAUS"
;;      "BIBELE"
;;      "ERDIORED"
;;      "ENMARI"
;;      "ANCEON"
;;      "CEBIIN"
;;      "MAERLAAT"
;;      "TIRISO"
;;      "XESOBIER"
;;      "RILAGETI"
;;      "ERINRAAT"
;;      "ENEDANCE"
;;      "ORQUQU"
;;      "GEINONER"
;;      "TEMAED"
;;      "GETE"
;;      "TEISQU"
;;      "AREDTI"
;;      "ARANON"
;;      "LEBIDIRA"
;;      "SOESBI"
;;      "SOZAGE"
;;      "VESOIN"
;;      "ISBEQU"
;;      "TITERI"
;;      "INTEANZA"
;;      "TEANTE"
;;      "INISONAR"
;;      "SOATXEAN"
;;      "ADITIBE"
;;      "EDZAAT"
;;      "GEGERA"
;;      "BEZABEMA"
;;      "GELEER"
;;      "SODIESRI"
;;      "LARIENBI"
;;      "TIATERQU"
;;      "TEBIIS"
;;      "ZAXESODI"
;;      "TIMAER"
;;      "ERLAORON"
;;      "REUSVEDI"
;;      "MAAUSGE"
;;      "ANSOSOAR"
;;      "ORMAA"
;;      "REATBEBE"
;;      "ORREUS"
;;      "TIDIDI"
;;      "REAAT"
;;      "AADI"
;;      "ESXETI"
;;      "RIQUTELA"
;;      "MAISON"
;;      "CEDIAN"
;;      "TIBI"
;;      "ZAUSMA"
;;      "DIENOR"
;;      "ZATILAEN"
;;      "ONSOED"
;;      "RILAENBI"
;;      "AESONED"
;;      "LEORRAA"
;;      "ISLEA"
;;      "ONTEAR"
;;      "DIRIENRI"
;;      "ARE"
;;      "RETEESRA"
;;      "ANQUARON"
;;      "GEDIATGE"
;;      "XETIIS"
;;      "DIGEESQU"
;;      "BIESAR"
;;      "REBETERI"
;;      "AAREDEN"
;;      "VEXEINCE"
;;      "LASOXEON"
;;      "ESMARA"
;;      "USBIEDSO"
;;      "BIBIRA"
;;      "QUAMA"
;;      "ERATED"
;;      "GEVERA"
;;      "EDQUON"
;;      "INISTEBI"
;;      "ONXEUS")
;;     (""
;;      ""
;;      "LEXE"
;;      "SODIED"
;;      "ENEDED"
;;      "RAERUSRE"
;;      "SORIRAAN"
;;      "REONESTI"
;;      "LEOR"
;;      "LAESQUOR"
;;      "ISATSO"
;;      "ADIBEA"
;;      "ERENEN"
;;      "ANGEBE"
;;      "ESAMAZA"
;;      "ONERLA"
;;      "ARZARA"
;;      "ISLAVE"
;;      "RAENA"
;;      "ORONA"
;;      "TIARE"
;;      "BETIIN"
;;      "CEENZATE"
;;      "ENBEXEUS"
;;      "RIARAT"
;;      "VEXERADI"
;;      "ZABIOR"
;;      "CEGEREVE"
;;      "USDIDI"
;;      "RIREA"
;;      "ANSOONLE"
;;      "SOUSEN"
;;      "ISATAT"
;;      "XEENRA"
;;      "AINCE"
;;      "BEORRE"
;;      "GEDIDI"
;;      "QUSOXE"
;;      "ARREANRA"
;;      "RIAGEEN"
;;      "CEQUEN"
;;      "ONZAVEOR"
;;      "RAZAEN"
;;      "GELEATVE"
;;      "ESERA"
;;      "ESLEBE"
;;      "RASOQUVE"
;;      "TIUSTI"
;;      "TIBILA"
;;      "ATIQU"
;;      "BEREXE"
;;      "LATELA"
;;      "EDRABE"
;;      "ESRIUSER"
;;      "EDENANCE"
;;      "ENBEINBE"
;;      "BEBEQU"
;;      "ERDIRI"
;;      "MAARRA"
;;      "ESUSTEDI"
;;      "ATANQU"
;;      "TEISON"
;;      "VERERIGE"
;;      "AATZA"
;;      "QUGEXE"
;;      "ZACEUS"
;;      "RIEDUS"
;;      "ONBIZA"
;;      "RAGEXE"
;;      "ISEDTI"
;;      "DIISXERE"
;;      "REISIN"
;;      "ARRAMA"
;;      "ESRIENQU"
;;      "MAEDIS"
;;      "TIVEATE"
;;      "XEDIIN"
;;      "TIBE"
;;      "ONRIESSO"
;;      "ALEOR"
;;      "ZASOVE"
;;      "LEQURI"
;;      "ENESLA"
;;      "ATEDAN"
;;      "ORQUED"
;;      "ONZAERLA"
;;      "MAATENA"
;;      "ENRAVEIS"
;;      "USISGE"
;;      "ARTIINRE"
;;      "ENDIA"
;;      "DIINARGE"
;;      "QUUSSO"
;;      "QUMAA"
;;      "ATVEISZA"
;;      "TIORVE"
;;      "TIRABE"
;;      "BITITE"
;;      "DIGEMA"
;;      "MAENQU"
;;      "CEEDTI"
;;      "BIRAERMA"
;;      "ERDIESSO"
;;      "RIERTISO"
;;      "ADIZA"
;;      "BEORREQU"
;;      "XEMAUS"
;;      "MAARDIGE"
;;      "ANMAES"
;;      "CERIBE"
;;      "ARVEEDOR"
;;      "SOORAN"
;;      "ATESTI"
;;      "BETEXE"
;;      "ATCEUS"
;;      "DILALE"
;;      "QUONIS"
;;      "LAMAEDOR"
;;      "RIATESED"
;;      "ENRAUS"
;;      "ISBIIN"
;;      "ZAXEEDON"
;;      "ORATAR"
;;      "ATATEDQU"
;;      "GEVERA"
;;      "EDQUON"
;;      "INISTEBI"
;;      "ONXEUS"
;;      "RABIZA"
;;      "USARER"
;;      "TEARE"
;;      "TIONER"
;;      "VEREIN"
;;      "INARUS"
;;      "BETEERLE"
;;      "RELEATLE"
;;      "RASOIS"
;;      "ISBEARAN"
;;      "ORONBE"
;;      "ONONMAES"
;;      "RAUSBI"
;;      "BEISBE"
;;      "LADISOAR"
;;      "ONENIS"
;;      "RIARED"
;;      "CEONBI"
;;      "ERAN"
;;      "ESATSO"
;;      "ANCEXE"
;;      "MAATRI"
;;      "ATERXE"
;;      "ENLAAREN"
;;      "ENERLA"
;;      "RIINZAA"
;;      "RACE"
;;      "LALABIER"
;;      "INGELE"
;;      "VEUSA"
;;      "SOCEANSO"
;;      "SOARQU"
;;      "ATTILA"
;;      "ARTEZA"
;;      "MAQUAT"
;;      "GEMABI"
;;      "SOGELE"
;;      "ENLERA"
;;      "VEINTIQU"
;;      "RIATMATE"
;;      "ORVE"
;;      "SOREBIAN"
;;      "ERENRI"
;;      "BEBEESER"
;;      "DIBIGE"
;;      "LEANBE"
;;      "CEVETE"
;;      "TIARON"
;;      "MAINOR"
;;      "TILEAR"
;;      "ATERE"
;;      "CEREDI"
;;      "TEARSO"
;;      "LEEDUSXE"
;;      "ZAERTIMA"
;;      "ENLARAIS"
;;      "SOORRI"
;;      "EDTIER"
;;      "SOED"
;;      "ORQURASO"
;;      "LAATRE"
;;      "RATION"
;;      "XEINQUES"
;;      "ABEIN"
;;      "REESBI"
;;      "INAED"
;;      "QUBIRA"
;;      "AEDTE"
;;      "EDORVE"
;;      "EDTIEDES"
;;      "ORANRIRA"
;;      "REXECEVE"
;;      "RIQUA"
;;      "ATUSXETE"
;;      "SOBIES"
;;      "SOUSSOVE"
;;      "BIGE"
;;      "AQUBE"
;;      "MATECEDI"
;;      "AGELE"
;;      "EDDIQU"
;;      "ESZARE"
;;      "RETILE"
;;      "LEARBE"
;;      "ISERIN"
;;      "TIRIERSO"
;;      "TIARELA"
;;      "ENVERISO"
;;      "TEGESO"
;;      "ENLETEER"
;;      "REANAN"
;;      "ISRIXEIS"
;;      "RIRIAN"
;;      "ENCEA"
;;      "ISRAORAR"
;;      "TIANRI"
;;      "MAQUTI"
;;      "RELEIN"
;;      "ESATTI"
;;      "ANZAEN"
;;      "ESREMA"
;;      "RIREERIS"
;;      "TEMACEDI"
;;      "RIENONAT"
;;      "GEBEUS"
;;      "ANGERITE"
;;      "RIORRA"
;;      "ANTECEIS"
;;      "ONLEIS"
;;      "ISEDBE"
;;      "VERARAON"
;;      "SOANGE"
;;      "CEAAN"
;;      "ANBIEN"
;;      "DIBERA"
;;      "ISSOOR"
;;      "RILAAT"
;;      "DICEEDMA"
;;      "ARACERI"
;;      "ENVEUSEN"
;;      "ATINER"
;;      "ININUSLE"
;;      "LAONVE"
;;      "LEGEERRA"
;;      "SOINUS"
;;      "ERRAON"
;;      "EDANEDIN"
;;      "ONZAER"))


;; (for [s (utility/galaxy-twist elite-seed (range 8))]
;;   (str s)
;;   )

;; (reduce (fn [a b]
;;           [b (map inc a)]) [0 0 0] [0 1 2])

;; (let [] elite-seed)
;; (planet-goat-soup elite-seed "TEST"
;;                   )
;; (utility/galaxy-twist)
;; (planet-goat-soup
;;  (utility/galaxy-twist
;;   (utility/galaxy-twist elite-seed))
;;  "test")

;; (for [p
;;       (take 9
;;             (iterate utility/galaxy-twist elite-seed))]
;;   (let []
;;     (println "-0-0-0-")
;;     (println (planet-goat-soup p "TEST"))
;;     (println (utility/get-seed-bytes p))
;;     )
;;   ))
