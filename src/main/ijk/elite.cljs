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

(bin-to-byte [1 1 1 1 1 1 1 1])

;;(map bin-to-byte(map byte-to-bin (get-seed-bytes (make-seed [9991494 14 98]))))

(defn get-seed-bits [seed byte-index start-index count-index]
  (subvec (into [] (nth (map byte-to-bin (get-seed-bytes seed)) byte-index))
          start-index
          (+ start-index count-index)))

(defn get-value-from-seed [seed byte-index start-index count-index]
  (let [array-of-bits (get-seed-bits seed byte-index start-index count-index)]
    (js/parseInt (cstring/join "" array-of-bits))))


(defn extract-seed-for-name-length
  "Given a planet seed, determine the length of the name (3 or 4 digraphs)"
  [seed]
  (if (get-value-from-seed seed (:s0_lo elite-index) 6 1)
    4
    3))


;; (extract-seed-for-name-length (make-seed [65035 14 98]))
;; =======
;;     4 3))

;;(bytes)
(get-seed-bytes
 (make-seed [65535 65535 65535]))



(defn generate-name-start [seed]
  (let [token-seed seed
        planet-name ""
        ]
    [token-seed planet-name]))

(defn generate-name [seed-token name-length name-in-progress]
  )



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
    (make-seed twisted)
    ))




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
(= '((0 1 0 0 1 0 1 0) (0 1 0 1 1 0 1 0) (0 1 0 0 1 0 0 0) (0 0 0 0 0 0 1 0) (0 1 0 1 0 0 1 1) (1 0 1 1 0 1 1 1))

   (map byte-to-bin (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753]))))
;; Test hyperjump
(= '("94" "b4" "90" "4" "a6" "6f")
 (map #(. % toString 16)
      (get-seed-bytes
       (hyperjump (make-seed [0x5A4A 0x0248 0xB753])))))






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
