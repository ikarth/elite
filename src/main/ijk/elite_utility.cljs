(ns ijk.elite-utility
  (:require 
   [clojure.edn :as edn]
   [clojure.string :as cstring]
   [grotesque.core :as grot]
   ;;[clojure.set]
   ;;["js-xxhash" :as xx :refer (xxHash32)]
   ;;["seedrandom" :as seedrandom]
   ;;[cljx-sampling.random :as random]
   ;;[cljx-sampling.core :refer [sample]]
   [rand-cljc.core :as rng]
   [cljs-node-io.core :as io :refer [slurp spit]]
   ))

(defn write-to-file
  ([data] (write-to-file data "output.edn"))
  ([data output-filename
    ]
   (if true ;; TODO: set to false if we're in a browser
     (let [] (spit output-filename data)
          "Wrote data to file."
          ))))

(defn positions
  "Get the indexes of items in a vector.
  https://stackoverflow.com/a/4831131/5562922"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn distance-2d [a b]
  (js/Math.sqrt
   (+ (js/Math.pow (- (first a) (first b)) 2)
      (js/Math.pow (- (second a) (second b)) 2))))

(defn distance-2d-bbc-elite
  "Because calculating the distance on a BBC Micro had limitations,
  we have to deliberately introduce a rounding error to match it."
  [location-one location-two]
  (let [x-delta (abs (- (first location-one) (first location-two)))
        x-delta (* x-delta x-delta)
        y-delta (/ (abs (- (second location-one) (second location-two))) 2)
        y-delta (* y-delta y-delta)
        delta (+ x-delta y-delta)
        hypot (js/Math.trunc (js/Math.sqrt delta)) ; deliberately truncate result
        dist (* 4 hypot)
        ]
    dist))

(comment
  (distance-2d-bbc-elite [20 173] [12 203])
  (distance-2d-bbc-elite [12 203] [19 236])

  (* 4 (distance-2d [12 (/ 203 2)] [19 118]))
  (distance-2d [12 105] [19 118])
  (/ 236 2))

;; The original Elite code makes extensive use of directly manipulating bits.
;; Which makes sense given the hardware that it was built for, but its
;; not something Javascript or Clojurescript are usually used to handle,
;; particularly with regard to the idiosyncrasies of the original hardware.
;; So we've got to write some functions to handle it.


;; These correspond to the registers in the BBC Micro code
(def elite-index
  {:s0_lo 0 ;; QQ15
   :s0_hi 1 ;; QQ15+1 = galactic-y
   :s1_lo 2 ;; QQ15+2
   :s1_hi 3 ;; QQ15+3 = galactic-x
   :s2_lo 4 ;; QQ15+4
   :s2_hi 5 ;; QQ15+5
  })


;; Seeds and Elite random numbers

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

(defn get-seed-bytes [seed]
  ;;{:pre [(spec/valid? :seed/data seed)]}
  (into [] (map #(. seed getUint8 %) (range 6) )))

(defn get-seed-bits [seed byte-index start-index count-index]
  (subvec (into [] (nth (map byte-to-bin (get-seed-bytes seed)) byte-index))
          start-index
          (+ start-index count-index)))

(defn make-seed [seed-vals]
  
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

(defn get-seed-byte-8 [seed byte-index]
  (. seed getUint8 byte-index))

(defn get-seed-byte-16 [seed byte-index]
  (. seed getUint16 byte-index))


(defn get-value-from-seed [seed byte-index start-index count-index]
  (let [array-of-bits (get-seed-bits seed byte-index start-index count-index)]
    (js/parseInt (cstring/join "" array-of-bits))))

;; Twisting random seeds



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


(defn galaxy-twist
  "Cycle the seed to get the next galaxy in the sequence"
  [old-seed]
  (let [new-seed
        (->> (get-seed-bytes old-seed)
             (mapv byte-to-bin)
             (mapv (fn [r] (concat (rest r) [(first r)])))
             (mapv bin-to-byte))]
    (make-seed
     [(+ (nth new-seed 0) (* 256 (nth new-seed 1)))
      (+ (nth new-seed 2) (* 256 (nth new-seed 3)))
      (+ (nth new-seed 4) (* 256 (nth new-seed 5)))
      ])))

;; (let []
;;   [0xB4 0x94 0x04 90 0x6FA6])
;; (byte-to-bin 0xB4)

;; (byte-to-bin 0x5A)
;; (byte-to-bin 0x4A)
;; (byte-to-bin 0x02)
;; (byte-to-bin 0x48)
;; (byte-to-bin 0xB7)
;; (byte-to-bin 0x53)

;; (byte-to-bin 74)
;; (byte-to-bin 2)
;; (byte-to-bin 83)

;; (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753]))
;; ;; => [74 90 72 2 83 183]

;; (let [sb (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753]))]
;;   (map #(nth sb %) [1 0 3 2 5 4])
;;   )
;; ;; => (90 74 2 72 183 83)
;; (mapv byte-to-bin (get-seed-bytes (make-seed [0x5A4A 0x0248 0xB753])))
;; ;; => [(0 1 0 0 1 0 1 0)
;; ;;     (0 1 0 1 1 0 1 0)
;; ;;     (0 1 0 0 1 0 0 0)
;; ;;     (0 0 0 0 0 0 1 0)
;; ;;     (0 1 0 1 0 0 1 1)
;; ;;     (1 0 1 1 0 1 1 1)]


;; (mapv byte-to-bin
;;       (get-seed-bytes
;;        (galaxy-twist (make-seed [0x5A4A 0x0248 0xB753]))))
;; ;; => [(0 0 1 0 0 1 0 1)
;; ;;     (0 0 1 0 1 1 0 1)
;; ;;     (0 0 1 0 0 1 0 0)
;; ;;     (0 0 0 0 0 0 0 1)
;; ;;     (1 0 1 0 1 0 0 1)
;; ;;     (1 1 0 1 1 0 1 1)]



(defn hyperjump [old-seed]
  (let [bits (map byte-to-bin (map #(. old-seed getUint8 %) (range 6)))
        after-jump (map (fn [seed]
                          (concat (rest seed) [(first seed)]))
                        bits)
        ]
    ;; (println bits)
    ;; (println after-jump)
    (bytes-to-seed (map bin-to-byte after-jump))))


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

(defn bitwise-add-vec
 "take two boolean vectors and add them, emulating the BBC micro behavior"
 [one two]
 (byte-to-bin
  (+
   (bin-to-byte one)
   (bin-to-byte two))))

(defn bitwise-and [one two]
  (mapv (fn [[a b]]
          (if (and (= a 1) (= b 1) ) 1 0))
        (map vector one two)))

(defn elite-bit-shift-left [data]
  (into [] cat [[] (drop 1 data) [0]]))

;; (defn goat-soup-next-rand [rand-seed]
;;   (let [f2 (elite-bit-shift-left (get rand-seed 0))
;;         carry-bit (get f2 0)
;;         f3 (+ (bin-to-byte (get rand-seed 0))
;;               (bin-to-byte (get rand-seed 2)) carry-bit)
;;         rand-a (byte-to-bin (bit-and f3 255))
;;         reg-a  (/ f3 256)
;;         x (bin-to-byte (get rand-seed 1))
;;         new-a (+ reg-a x (bin-to-byte (get rand-seed 3)))
;;         new-a-bit (byte-to-bin (bit-and new-a 255))
;;         new-seed [rand-a new-a-bit f2 (get rand-seed 1)]
;;         ]
;;     [(bin-to-byte new-a-bit) new-seed]))


(defn goat-soup-next-rand [rand-seed]
  (let [int-seed (mapv bin-to-byte rand-seed)
        x (* 2 (nth int-seed 0))
        ;;_ (println "x " x (rem x 256))
        x (bit-and x 0xFF)
        ;;_ (println "x " x)
        a (+ (nth int-seed 2) x)
        ;;_ (println "a " a)        
        a (if (> (nth int-seed 0) 127)
            (+ a 1)
            a)
        ;;_ (println "a " a)                
        one (bit-and a 0xFF)
        three x
        ;;_ (println "one " one)                
        a-carry  (quot a 256)
        ;;_ (println "a-carry " a-carry)        
        x2 (nth int-seed 1)
        a2 (+ a-carry x2 (nth int-seed 3))
        ;;_ (println "a2 " a2)        
        a2 (bit-and a2 0xFF)
        two a2        
        four x2
        new-seed [one two three four]]
    ;;(println new-seed)
    [two (mapv byte-to-bin new-seed);; new-seed
     ]))

;; (quot 3000 256)

;; (goat-soup-next-rand (mapv byte-to-bin [156 185 144   2]))
;; => [58  [(1 1 0 0 1 0 0 1) (0 0 1 1 1 0 1 0) (0 0 1 1 1 0 0 0) (1 0 1 1 1 0 0 1)] [201 58 56 185]]
;; => [188 [(0 0 1 0 1 1 0 0) (1 0 1 1 1 1 0 0) [0 0 1 1 1 0 0 0] (1 0 1 1 1 0 0 1)]]
;; => [201 [201 201 56 185]]
;;; 
;; (goat-soup-next-rand (mapv byte-to-bin [201 201 56 185]))

;; (update-in (goat-soup-next-rand (mapv byte-to-bin [156 185 144   2])) [1] (fn [x] (mapv bin-to-byte x)))
;; (update-in (goat-soup-next-rand (mapv byte-to-bin [100 117  88 188])) [1] (fn [x] (mapv bin-to-byte x)))

