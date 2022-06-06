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

(def database-records
  {})

(defonce current-database
  (atom {:db-conn nil
         :db-schema {}}))


(defn setup [])

(defn update-database [database-id]
  (let [new-database (get database-records database-id :default-database)]
    (if (and new-database (get new-database :db-conn false))
      (let [swap-in! (fn [db-id]
                       (swap! current-database assoc-in [db-id]
                              (get new-database db-id)))]
        (swap-in! :db-conn)
        (swap-in! :db-schema)
        )
      (println (str "Database not found: " database-id)))))

(defn reset-the-database!
  [database-id]
  (update-database database-id)
  (if-let [db-conn (get @current-database :db-conn)]
    (let [db-schema (get @current-database :db-conn)]
      (d/reset-conn! db-conn (d/empty-db db-schema))
      ;; TODO: any initial transactions go here...
      db-conn
      )))

(defn fetch-internal-view
  "Returns the internal database. Intended mostly for debugging visualization."
  []
  (if-let [db-conn (get @current-database :db-conn)]
    (vec (map (fn [dat]
                (let [[e a v tx add] dat]
                  [e a v tx add])) (d/datoms @db-conn :eavt)))
    (println "Database connection missing when trying to fetch a new view.")))

(defn make-empty-project [database-id]
  (println (str "Switching to: " database-id))
  (assert (not (undefined? database-id)) "Tried to switch to an undefined generator database.")
  (update-database database-id)
  (println "Resetting database...")
  (reset-the-database! database-id))

(spec/def :seed/specifier
  (spec/and
   (spec/coll-of number? :kind vector? :count 3 :into [])
   ;(spec/every #(>= 0 % 255) :count 6)
   ))























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

(defn make-seed [seed-vals]
  {:pre  [(spec/valid? :seed/specifier seed-vals)]
   ;;:post [(spec/valid? :seed/data-array %)]
   }
  (let [ab (js/ArrayBuffer. 6)
      view (js/DataView. ab)]
  (. view setUint16 0 (nth seed-vals 0) true)    
  (. view setUint16 2 (nth seed-vals 1) true)    
  (. view setUint16 4 (nth seed-vals 2) true) 
  view))

(defn bytes-to-seed [seed-bytes]
  (let [ab (js/ArrayBuffer. 6)
        view (js/DataView. ab)]
    (doseq [n (range 6)]
      (. view setUint8 n (nth seed-bytes n) true))
    view))

;; (bytes-to-seed [0 1 2 3 4 5])
;; (get-seed-bytes (bytes-to-seed [0 10 20 30 40 50]))


(defn get-seed-bytes [seed]
  ;;{:pre [(spec/valid? :seed/data seed)]}
  (into [] (map #(. seed getUint8 %) (range 6) )))

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
   "" ;; for planet names, 0 is a special case. I'm encoding this in this table instead of in the function.
   ;;"AL"              ; Token 128
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


(defn generate-name [input]
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


  ;; ([number-byte]
  ;;  (if #(= 0 number-byte)
  ;;    (invert-byte number-byte 3)
  ;;    (first (positions #{1} col))))

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
        (get-seed-bits planet-seed
                       (:s0_hi elite-index)
                       5 3)
        adjusted (assoc eco-base 1 (if (< planet-government 2) 1 (nth eco-base 1)))
        type (nth eco-base 0)
        prosperity (bin-to-byte adjusted)
        ;; Returning both the prosperity-coerced-to-an-integer *and* the flipped number
        ;; is an example of how the original code compactly reuses its computational
        ;; resources, in contrast with how our more-modular reconstruction needs to
        ;; separate the values. I could avoid this if I just kept it as the bytes
        ;; (or if I had a byte-flipping function)
        ;;flipped-economy (bin-to-byte (invert-bits adjusted))
        ;; ...on second thought, I wrote the byte-flipping function.
        ]
    (println [eco-base adjusted prosperity])
    [type prosperity]))


(defn planet-tech-level
  "The tech level formula is:
       flipped_economy + (s1_hi AND %11) + (government / 2)" 
  [planet-seed economy government]
  (+ (invert-byte (second economy) 3)
     (bin-to-byte
      (get-seed-bits planet-seed (:s1_hi elite-index) 6 2))
     (clojure.math/ceil (/ government 2))))


(def species-table
  [["Large ", "Fierce ", "Small ", "", "", "", "", ""]
  ["Green " "Red " "Yellow" "Blue " "Black " "Harmless " "" "" "" ""]
  ["Slimy " "Bug-eyed " "Horned " "Bony " "Fat ", "Furry ", "", ""]
  ["Rodents"
   "Frogs"
   "Lizards"
   "Birds"
   "Humanoids"
   "Felines"
   "Insects"]
   ])

(defn bitwise-add-vec
 "take two boolean vectors and add them, emulating the BBC micro behavior"
 [one two]
 ;; (reduce
 ;;  (fn [construct adders]
 ;;    (let [new-sum
 ;;          (map + adders)]
 ;;      )
 ;;    )
 ;;  [] (interleave (reverse one) (reverse two)))
 ;; [ 0 0 0 0  0 0 0 0]
 (byte-to-bin
  (+
   (bin-to-byte one)
   (bin-to-byte two))))

;; 01234567
;; 76543210

(defn planet-species
  [seed]
  ;; (println (bin-to-byte (get-seed-bits seed (:s2_lo elite-index) 0 8))
  ;;          (get-seed-bits seed (:s2_lo elite-index) 0 1))
  ;; (println [(into []
  ;;                 (byte-to-bin
  ;;                  (bit-xor
  ;;                   (bin-to-byte (get-seed-bits seed (:s0_hi elite-index) 0 8))
  ;;                   (bin-to-byte (get-seed-bits seed (:s1_hi elite-index) 0 8)))))
  ;;           (byte-to-bin
             
  ;;            (bin-to-byte (get-seed-bits seed (:s0_hi elite-index) 0 8)))
  ;;           (byte-to-bin (bin-to-byte (get-seed-bits seed (:s1_hi elite-index) 0 8)))
          

  ;;           ]
  ;;         )
  (if (= 0 
         (first (get-seed-bits seed (:s2_lo elite-index) 0 1))
         )
    "Human Colonials"
    (let [register-A 
          (vec (concat [0 0]
                       (into [] (get-seed-bits seed (:s2_hi elite-index) 0 6))))
          species-size  (bin-to-byte (mapv *
                                          [0 0 0 0 0 1 1 1]
                                          register-A))
          species-color (subvec register-A 3 5)          
          texture (mapv * [0 0 0 0 0 1 1 1]
                       (mapv (fn [a b] (if (not= a b) 1 0))
                            (get-seed-bits seed (:s0_hi elite-index) 0 8)
                            (get-seed-bits seed (:s1_hi elite-index) 0 8)))
          species-name (mapv * [0 0 0 0 0 1 1 1]
                             (bitwise-add-vec
                              (mapv * [0 0 0 0 0 0 1 1] register-A)
                              texture))
          
          type (bin-to-byte (get-seed-bits seed (:s2_hi elite-index) 6 2))
          genus (mapv * [0 0 0 0 0 0 1 1] (get-seed-bits seed (:s2_hi elite-index) 0 8))
          species-id [(bin-to-byte (get-seed-bits seed (:s2_hi elite-index) 3 3)) ;; size
                      (bin-to-byte (get-seed-bits seed (:s2_hi elite-index) 0 3)) ;; color
                      (bin-to-byte (subvec texture 5)) ;; texture
                      ;; (bin-to-byte (subvec (into [] (byte-to-bin (+ (bin-to-byte (subvec texture 6))
                      ;;                                               (bin-to-byte (subvec texture 5))))) 5))
                      (bin-to-byte (mapv * [0 0 0 0 0 1 1 1] (into [] (byte-to-bin (+ (bin-to-byte (subvec texture 5)) (bin-to-byte genus))))))

                      ]] ;; type
      (comment )
      (println ["\n* * * *\n"
                register-A "\n"
                (get-seed-bits seed (:s2_hi elite-index) 0 8) "\n"
                (get-seed-bits seed (:s0_hi elite-index) 0 8) "\n"
                (get-seed-bits seed (:s1_hi elite-index) 0 8) "\n"
                texture "\n"
                genus "\n"
                texture "+" genus "=" (byte-to-bin (+ (bin-to-byte texture) (bin-to-byte genus))) "\n"
                (bin-to-byte  texture) " + " (bin-to-byte genus)
                " = "
                (+ (bin-to-byte texture) (bin-to-byte genus)) "\nOR\n"
                (bin-to-byte (mapv * [0 0 0 0 0 1 1 1] (into [] (byte-to-bin (+ (bin-to-byte (subvec texture 5)) (bin-to-byte genus)))))) "\n"
                (subvec (into [] (byte-to-bin (+ (bin-to-byte texture) (bin-to-byte genus)))) 4) "\n"
                (apply str        (map #(get %2 %1) species-id species-table)) "\n"
                ;;(byte-to-bin (+ (bin-to-byte texture) (bin-to-byte genus))) "\n"
                ;; genus
                ;; texture type
                ;; "\n\n"
                ;; genus
                ;; "\n>>>"
                ;; species-id
                ;;   ;;(subvec texture 5)
                ;;   ;;(subvec (into [] (byte-to-bin (+ type (bin-to-byte texture)))) 5)
                ;;   "\n=>\n"
                ;;   (get-seed-bits seed (:s2_hi elite-index) 0 8)
                ;;   type
                ;;   (get-seed-bits seed (:s2_hi elite-index) 6 2)
                ;;   "\n+\n"
                ;;   texture
                ;;   (bin-to-byte texture)
                ;;   (bin-to-byte (subvec texture 6))
                ;;   "\n=\n"
                ;; (+ (bin-to-byte (subvec texture 5)) (bin-to-byte (subvec texture 6)))
                ;; (byte-to-bin (+ (bin-to-byte (subvec texture 5)) (bin-to-byte (subvec texture 6))))
                ;; (bin-to-byte (subvec (into [] (byte-to-bin (+ (bin-to-byte (subvec texture 6))
                ;;                                               (bin-to-byte (subvec texture 5))))) 5))
                ;; (subvec (into [] (byte-to-bin (+ (bin-to-byte (subvec texture 6))
                ;;                                                (bin-to-byte (subvec texture 5))))) 5)
                  ])
      (apply str 
       (map #(get %2 %1) species-id species-table)))))


(planet-species (make-seed [0x57fa 0x1d30 0x17b3]))
(planet-species (make-seed [0x588a 0x476c 0x02db]))

(last (take 6
            (iterate
             generate-name (generate-name-start (make-seed [0xfa57 0x301d 0xb317]))
             
             )))
(last (take 6
            (iterate
             generate-name (generate-name-start (make-seed [0x57fa 0x1d30 0x17b3]))
             
             )))


;; [0 0 0 0 0 0 1 0] ;; s2
;; [0 1 0 1 1 0 0 0] ;; s0
;; [0 1 0 0 0 1 1 1] ;; s1
;; [0 0 0 1 1 1 1 1] ;; XOR
;; [0 0 1 0 0 0 0 1] ;; A + B


;; [0 0 0 1 1 1 1 1] 2
;; s2_hi
;; s0_hi
;; s1_hi
;; A = s0_hi EOR s1_hi
;; s2_hi + A

;; 0 0 0 0 - 0 0 1 0 
;;

;; 0 0 1 1
;; 0 1 0 1 +
;; 1 0 0 0 =


;;(planet-species elite-seed)

;; (get-seed-bits elite-seed (:s1_hi elite-index) 0 8)

;; (planet-tech-level47
;;  elite-seed
;;  (planet-economy elite-seed (planet-government elite-seed))
;;  (planet-government elite-seed))

;; (planet-tech-level
;;  planet-two
;;  (planet-economy planet-two (planet-government planet-two))
;;  (planet-government planet-two))


; 0 - 7 , 0 - 3, 0 - 3.5
;; (let [sb (get-seed-bytes elite-seed)]
;;   [
;;    (bit-and (nth sb (:s0_hi elite-index)) 2r00000111)
;;    (bit-xor (second (planet-economy elite-seed (planet-government elite-seed))) 2r111)
;;    (invert-byte (second (planet-economy elite-seed (planet-government elite-seed))) 3)
;;    (bit-and (nth sb (:s1_hi elite-index)) 2r00000011)
;;    (planet-government elite-seed)
;;    (bit-and (nth sb (:s1_lo elite-index)) 2r00000011)
;;    ]
;;   )

;; (bin-to-byte
;;  (get-seed-bits elite-seed (:s1_hi elite-index) 0 8))

(defn planet-population-size [tech-level economy government]
  (+
   (* tech-level 4)
   (second economy)
   government
   1))

(defn planet-productivity [economy government population]
  (* 8
     (+ (invert-byte (second economy) 3) 3)
     (+ government 4)
     population
     ))


(defn government-name [gov-type]
  (nth [:anarchy :feudal :multi-government :dictatorship :communist :confederacy :democracy :corporate-state]
       gov-type))

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


;; (bin-to-byte [0 1 0 0])

;; (map byte-to-bin (get-seed-bytes elite-seed))
;; (government-name (planet-government elite-seed))
;; (economy-name (planet-economy elite-seed (planet-government elite-seed)))
;; (planet-tech-level
;;  elite-seed
;;  (planet-economy elite-seed (planet-government elite-seed))
;;  (planet-government elite-seed))

;; (map byte-to-bin (get-seed-bytes planet-two))
;; (government-name (planet-government planet-two))
;; (economy-name
;;  (planet-economy planet-two (planet-government planet-two)))
;; (planet-tech-level
;;  planet-two
;;  (planet-economy planet-two (planet-government planet-two))
;;  (planet-government planet-two))





;; (get-seed-bytes (nth planet-seed-list 0))
;; (get-seed-bytes (nth planet-seed-list 1))

;; (map-indexed (fn [index item]
;;                [index (get-seed-bytes item)])
;;              planet-seed-list)


(def elite-seed (make-seed [0x5A4A 0x0248 0xB753]))
(def planet-two (-> elite-seed
                    twist-seed
                    twist-seed
                    twist-seed
                    twist-seed))

(def planet-seed-list
  (reduce (fn [current next-id] (concat current [(twist-to-next-planet (last current))]))
          [elite-seed]
          (range 255)))


(def auth-tech [9 7 8 12 7 10 9 5 12 7 3 10 9 7 7 8 9 8 7 7 9 4 11 14 8])


(let [[a b c] (generate-name-start elite-seed)]
  (generate-name [a b c]))

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
        (if (< 245 r )
          (println 
           (map 
            #(str %1 ":\t "%2 "\n")
            ["id" "name" "seed" "species" "government" "economy" "tech-level" "population size" "productivity"]
            [r
             name
                                        ;(map byte-to-bin (get-seed-bytes p))
             (map #(. % toString 16) (get-seed-bytes p))
             species
             (government-name gov)
             (economy-name econ)
             tech
             pop
             prod
             ])))))))

;;(test-galaxy-generator )




(def generative-operations
  [;;{:op-function generate-galaxy}
   {:op-function make-seed}
   ;;{:op-function next-planet}
   {:input :seed/planet-seed
    :output :planet/government-type 
    :op-function planet-government}
   {:op-function planet-economy}
   {:op-function planet-tech-level}
   {:op-function planet-population-size}
   {:op-function planet-productivity}
   {:op-function generate-name-start}
   {:op-function generate-name}
   {:op-function planet-species}
   ]
  )







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
