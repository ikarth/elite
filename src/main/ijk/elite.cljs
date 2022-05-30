(ns ijk.elite
  (:require
   [ijk.elite-grammar :as egrammar]
   [datascript.core :as d]
   [clojure.spec.alpha :as spec]
   ;;[clojure.edn :as edn]
   [clojure.string :as cstring]
   ;;[grotesque.core :as grot]
   ;;["js-xxhash" :as xx :refer (xxHash32)]

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log-db
  "Log a complete listing of the entities in the provided `db` to the console."
  [db]
  (d/q '[:find ?any ?obj :where [?obj :type ?any]] @db)) ;todo: log to console...



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

;;(byte-array [(byte 0x43)])



;; (. (js/DataView. (js/ArrayBuffer. 2))
;;    setInt16 0 255)


;; (let [ab (js/ArrayBuffer. 6)
;;       view (js/DataView. ab)
;;       ]
;;   (. view setUint16 1 65535 true)    
;;   [(. view getUint8 0 )
;;    (. view getUint8 1 )
;;    (. view getUint8 2 )
;;    (. view getUint8 3 )
;;    (. view getUint8 4 )
;;    (. view getUint8 5 )
;;    ]
;;   )

(spec/def :seed/specifier
  (spec/and
   (spec/coll-of number? :kind vector? :count 3 :into [])
   ;(spec/every #(>= 0 % 255) :count 6)
   ))

;; (spec/def :seed/data-array
;;   (spec/and
   
;;    ))

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

(defn get-seed-bytes [seed]
  (into [] (map #(. seed getUint8 %) (range 6) )))

(get-seed-bytes (make-seed [65535 65535 65535]))

(defn byte-to-bin [dec]
  (let [byte-length 8
        num-vec (vec (. (bit-shift-right dec 0) toString 2))
        extra (- byte-length (count num-vec))
        left-pad (take extra (repeat 0))
        byte-vec (concat left-pad num-vec)
        ]
    (map #(js/parseInt % 2) byte-vec
         )))

(map byte-to-bin (get-seed-bytes (make-seed [9991494 14 98])))

(defn get-seed-bits [seed byte-index start-index count-index]
  (subvec (into [] (nth (map byte-to-bin (get-seed-bytes seed)) byte-index))
          start-index
          (+ start-index count-index))
  ;;(nth (byte-to-bin seed) bit-index)
  )

(subvec
 (into []
       (nth 
        (map byte-to-bin (get-seed-bytes (make-seed [1494 14 98])))
        0))
 0
 )


(get-seed-bytes (make-seed [1494 14 98]))

(byte-to-bin (make-seed [1494 14 98]))
(get-seed-bits (make-seed [1494 14 98]) 0 1 2)

(defn get-value-from-seed [seed byte-index start-index count-index]
  (let [array-of-bits (get-seed-bits seed byte-index start-index count-index)]
    (js/parseInt (cstring/join "" array-of-bits))
    ))

(get-value-from-seed (make-seed [14499 14 98]) 0 1 5)












;;(bit-shift-right (bytes 2r1001) 1)

;; (defn binary-string
;;   "Returns a binary representation of a byte value."
;;   [x]
;;   (let [s #?(:clj (Integer/toBinaryString x)
;;              :cljs (.toString x 2))
;;         c (count s)]
;;     (if (< c 8)
;;       (str (apply str (repeat (- 8 c) "0")) s)
;;       (subs s (- (count s) 8) (count s)))))

(defn display-seed [seed]
  (doseq [s-index (keys seed)]
    (println (get seed s-index nil))
      )
  )

(defn create-zero-seed
  []
  {:s0_lo (byte 2r00000000) ;; QQ15
   :s0_hi (byte 2r00000000) ;; QQ15+1
   :s1_lo (byte 2r00000000) ;; QQ15+2
   :s1_hi (byte 2r00000000) ;; QQ15+3
   :s2_lo (byte 2r00000000) ;; QQ15+4
   :s2_hi (byte 2r00000000) ;; QQ15+5
  })

(defn low-byte [byte16]
  (bit-and 0xff byte16))
(defn high-byte [byte16]
  (bit-and 0xff (bit-shift-right byte16 8)))
(defn get-bit [byte8 bit])

(low-byte (byte-array 2025))
(high-byte (byte-array 2025))
(. (high-byte 2025) toString)

(defn create-seed [seed-val]
  {:s0_lo (byte (nth seed-val 0)) ;; QQ15
   :s0_hi (byte (nth seed-val 0)) ;; QQ15+1
   :s1_lo (byte (nth seed-val 1)) ;; QQ15+2
   :s1_hi (byte (nth seed-val 1)) ;; QQ15+3
   :s2_lo (byte (nth seed-val 2)) ;; QQ15+4
   :s2_hi (byte (nth seed-val 2)) ;; QQ15+5
  }
  )

(create-seed [4 5 346])


(defn extract-seed-for-name-length
  "Given a planet seed, determine the length of the name (3 or 4 digraphs)"
  [seed]


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
