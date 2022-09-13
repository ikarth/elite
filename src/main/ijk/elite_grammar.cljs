(ns ijk.elite-grammar
  (:require 
   [clojure.edn :as edn]
   [clojure.string :as cstring]
   [grotesque.core :as grot]
    [ijk.elite-utility :as utility]
   ;;[clojure.set]
   ["js-xxhash" :as xx :refer (xxHash32)]
   ["seedrandom" :as seedrandom]
   ;;[cljx-sampling.random :as random]
   ;;[cljx-sampling.core :refer [sample]]
   [rand-cljc.core :as rng]
   ))

;; (let [s (seedrandom. "tes6t")]
;;   (println (js-str (s)))
;;   ;;(js-str (.log js/console s))
;;   )

(defn equb-range
  "Returns a list of numbers from start to start+4 (inclusive), for use in constructing Elite's grammar rules"
  [start]
  (into [] (map (fn [n] (str "<TKN1_" n ">")) (take 5 (range start (+ start 5))))))

(equb-range 6)


(def two-letter-tokens
  {215     "{crlf}"
   216     "AB"
   217     "OU"
   218     "SE"
   219     "IT"
   220     "IL"
   221     "ET"
   222     "ST"
   223     "ON"
   224     "LO"
   225     "NU" ; 225
   226     "TH"
   227     "NO"
   228     "AL"
   229     "LE"
   230     "XE"
   231     "GE"
   232     "ZA"
   233     "CE"
   234     "BI"
   235     "SO"
   236     "US"
   237     "ES"
   238     "AR" ; 238
   239     "MA"
   240     "IN"
   241     "DI"
   242     "RE"
   243     "A"
   244     "ER"
   245     "AT"
   246     "EN"
   247     "BE"
   248     "RA"
   249     "LA"
   250     "VE"
   251     "TI"
   252     "ED"
   253     "OR"
   254     "QU"
   255     "AN"
   })

(def text-tokens-extended
{
:TKN1_0 ""
:TKN1_1 "main menu"
:TKN1_2 "\nWHICH DRIVE?"
:TKN1_3 "COMPETITION NUMBER:"
:TKN1_4 "{clear screen}{draw box around title}{all caps}{tab 6}DRIVE {drive number} CATALOGUE{crlf}"
:TKN1_5 "<TKN1_176><EQUB_86><TKN1_202><EQUB_140>.\n" ; "{lower-case}{justify}{single cap}<EQUB_86> IS <EQUB_140>.\n{left align}"
:TKN1_6 "  LOAD NEW {single cap}COMMANDER {all caps}(Y/N)?#sentence-case#\n\n"
:TKN1_7 "PRESS SPACE OR FIRE,{single cap}COMMANDER.\n\n"
:TKN1_8 "{single cap}COMMANDER'S NAME? "
:TKN1_9 "{clear bottom of screen}FILE TO DELETE?"
:TKN1_10 "MISSION TEXT GOES HERE"
:TKN1_11 "MISSION TEXT GOES HERE"
:TKN1_12 "MISSION TEXT GOES HERE"
:TKN1_13 "MISSION TEXT GOES HERE"
:TKN1_14 "MISSION TEXT GOES HERE"
:TKN1_15 "MISSION TEXT GOES HERE"
:TKN1_16 "FABLED"
:TKN1_17 "NOTABLE"
:TKN1_18 "WELL KNOWN"
:TKN1_19 "FAMOUS"
:TKN1_20 "NOTED"
:TKN1_21 "VERY"
:TKN1_22 "MILDLY"
:TKN1_23 "MOST"
:TKN1_24 "REASONABLY"
:TKN1_25 ""
:TKN1_26 "ANCIENT"
:TKN1_27 "<EQUB_130>"
:TKN1_28 "GREAT"
:TKN1_29 "VAST"
:TKN1_30 "PINK"
:TKN1_31 "{sentence-case}<EQUB_190> <EQUB_185> {lower-case}PLANTATIONS"
:TKN1_32 "MOUNTAINS"
:TKN1_33 "<EQUB_180>"
:TKN1_34 "<EQUB_125> FORESTS"
:TKN1_35 "OCEANS"
:TKN1_36 "SHYNESS"
:TKN1_37 "SILLINESS"
:TKN1_38 "MATING TRADITIONS"
:TKN1_39 "LOATHING OF <EQUB_41>"
:TKN1_40 "LOVE FOR <EQUB_41>"
:TKN1_41 "FOOD BLENDERS"
:TKN1_42 "TOURISTS"
:TKN1_43 "POETRY"
:TKN1_44 "DISCOS"
:TKN1_45 "<EQUB_81>"
:TKN1_46 "WALKING TREE"
:TKN1_47 "CRAB"
:TKN1_48 "BAT"
:TKN1_49 "LOBST"
:TKN1_50 "<GENR_8>" ; {18}
:TKN1_51 "BESET"
:TKN1_52 "PLAGUED"
:TKN1_53 "RAVAGED"
:TKN1_54 "CURSED"
:TKN1_55 "SCOURGED"
:TKN1_56 "<EQUB_135> CIVIL WAR"
:TKN1_57 "<EQUB_170> <EQUB_155> <EQUB_160>S"
:TKN1_58 "A <EQUB_170> DISEASE"
:TKN1_59 "<EQUB_135> EARTHQUAKES"
:TKN1_60 "<EQUB_135> SOLAR ACTIVITY"
:TKN1_61 "ITS <EQUB_26> <EQUB_31>"
:TKN1_62 "THE <GENR_system_adjective> <EQUB_155> <EQUB_160>"
:TKN1_63 "ITS INHABITANTS' <EQUB_165> <EQUB_36>"
:TKN1_64 "{sentence-case}<EQUB_235>{lower-case}"
:TKN1_65 "ITS <EQUB_76> <EQUB_81>"
:TKN1_66 "JUICE"
:TKN1_67 "BRANDY"
:TKN1_68 "WATER"
:TKN1_69 "BREW"
:TKN1_70 "GARGLE BLASTERS"
:TKN1_71 "<GENR_8>"
:TKN1_72 "<GENR_system_adjective> <EQUB_160>"
:TKN1_73 "<GENR_system_adjective> <GENR_8>"
:TKN1_74 "<GENR_system_adjective> <EQUB_170>" ; "{17}"
:TKN1_75 "<EQUB_170> <GENR_8>"
:TKN1_76 "FABULOUS"
:TKN1_77 "EXOTIC"
:TKN1_78 "HOOPY"
:TKN1_79 "UNUSUAL"
:TKN1_80 "EXCITING"
:TKN1_81 "CUISINE"
:TKN1_82 "NIGHT LIFE"
:TKN1_83 "CASINOS"
:TKN1_84 "SIT COMS"
:TKN1_85 "{sentence-case}<EQUB_235>{lower-case}"
:TKN1_86 "<GENR_system_name>"
:TKN1_87 "THE PLANET <GENR_system_name>"
:TKN1_88 "THE WORLD <GENR_system_name>"
:TKN1_89 "THIS PLANET"
:TKN1_90 "THIS WORLD"
:TKN1_91 "SON OF A BITCH"
:TKN1_92 "SCOUNDREL"
:TKN1_93 "BLACKGUARD"
:TKN1_94 "ROGUE"
:TKN1_95 "WHORESON BEETLE HEADED FLAP EAR'D KNAVE"
:TKN1_96 "N UNREMARKABLE"
:TKN1_97 " BORING"
:TKN1_98 " DULL"
:TKN1_99 " TEDIOUS"
:TKN1_100 " REVOLTING"
:TKN1_101 "PLANET"
:TKN1_102 "WORLD"
:TKN1_103 "PLACE"
:TKN1_104 "LITTLE PLANET"
:TKN1_105 "DUMP"
:TKN1_106 "I HEAR A <EQUB_130> LOOKING SHIP APPEARED AT ERRIUS"
:TKN1_107 "YEAH, I HEAR A <EQUB_130> SHIP LEFT ERRIUS A WHILE BACK"
:TKN1_108 "GET YOUR IRON ASS OVER TO ERRIUS"
:TKN1_109 "SOME <EQUB_91> NEW SHIP WAS SEEN AT ERRIUS"
:TKN1_110 "TRY ERRIUS"
:TKN1_111 ""
:TKN1_112 ""
:TKN1_113 ""
:TKN1_114 ""
:TKN1_115 "WASP"
:TKN1_116 "MOTH"
:TKN1_117 "GRUB"
:TKN1_118 "ANT"
:TKN1_119 "<GENR_8>"
:TKN1_120 "POET"
:TKN1_121 "ARTS GRADUATE"
:TKN1_122 "YAK"
:TKN1_123 "SNAIL"
:TKN1_124 "SLUG"
:TKN1_125 "TROPICAL"
:TKN1_126 "DENSE"
:TKN1_127 "RAIN"
:TKN1_128 "IMPENETRABLE"
:TKN1_129 "EXUBERANT"
:TKN1_130 "FUNNY"
:TKN1_131 "WIERD"
:TKN1_132 "UNUSUAL"
:TKN1_133 "STRANGE"
:TKN1_134 "PECULIAR"
:TKN1_135 "FREQUENT"
:TKN1_136 "OCCASIONAL"
:TKN1_137 "UNPREDICTABLE"
:TKN1_138 "DREADFUL"
:TKN1_139 "DEADLY"
:TKN1_140 "<EQUB_21> <EQUB_16> FOR <EQUB_61>"
:TKN1_141 "<TKN1_140><TKN1_178><EQUB_61>"
:TKN1_142 "<EQUB_51> BY <EQUB_56>"
:TKN1_143 "<TKN1_140> BUT <TKN1_142>"
:TKN1_144 "A<EQUB_96> <EQUB_101>"
:TKN1_145 "PLANET"
:TKN1_146 "WORLD"
:TKN1_147 "THE "
:TKN1_148 "THIS "
:TKN1_149 "LOAD NEW {single cap}COMMANDER"
:TKN1_151 "DRIVE"
:TKN1_152 " CATALOGUE"
:TKN1_153 "IAN"
:TKN1_154 "{single cap}COMMANDER"
:TKN1_155 "<EQUB_170>"
:TKN1_156 "MOUNTAIN"
:TKN1_157 "EDIBLE"
:TKN1_158 "TREE"
:TKN1_159 "SPOTTED"
:TKN1_160 "<EQUB_225>"
:TKN1_161 "<EQUB_230>"
:TKN1_162 "<EQUB_46>OID"
:TKN1_163 "<EQUB_120>"
:TKN1_164 "<EQUB_115>"
:TKN1_165 "ANCIENT"
:TKN1_166 "EXCEPTIONAL"
:TKN1_167 "ECCENTRIC"
:TKN1_168 "INGRAINED"
:TKN1_169 "<EQUB_130>"
:TKN1_170 "KILLER"
:TKN1_171 "DEADLY"
:TKN1_172 "EVIL"
:TKN1_173 "LETHAL"
:TKN1_174 "VICIOUS"
:TKN1_175 "ITS "
:TKN1_176 "{lower-case}{justify}{single cap}"
:TKN1_177 ".\n{left align}"
:TKN1_178 " AND "
:TKN1_179 "YOU"
:TKN1_180 "PARKING METERS"
:TKN1_181 "DUST CLOUDS"
:TKN1_182 "ICE BERGS"
:TKN1_183 "ROCK FORMATIONS"
:TKN1_184 "VOLCANOES"
:TKN1_185 "PLANT"
:TKN1_186 "TULIP"
:TKN1_187 "BANANA"
:TKN1_188 "CORN"
:TKN1_189 "<GENR_8>WEED"
:TKN1_190 "<GENR_8>"
:TKN1_191 "<GENR_system_adjective> <GENR_8>"
:TKN1_192 "<GENR_system_adjective> <EQUB_170>"
:TKN1_193 "INHABITANT"
:TKN1_194 "<TKN1_191>"
:TKN1_195 "ING "
:TKN1_196 "ED "
:TKN1_197 ""
:TKN1_198 ""
:TKN1_199 ""
:TKN1_200 " NAME? "
:TKN1_201 " TO "
:TKN1_202 " IS "
:TKN1_203 "WAS LAST SEEN AT {single cap}"
:TKN1_204 ".\n {single cap}"
:TKN1_205 "DOCKED"
:TKN1_206 "{all caps}(Y/N)?"
:TKN1_207 "SHIP"
:TKN1_208 " A "
:TKN1_209 " ERRIUS"
:TKN1_210 " NEW "
:TKN1_211 "{sentence-case} HER MAJESTY'S SPACE NAVY{lower-case}"
:TKN1_212 ".\n{left align}{tab 6}{all caps}  MESSAGE ENDS"
:TKN1_213 " {single cap}COMMANDER {commander name}, I {lower-case}AM{sentence-case} CAPTAIN {mission captain's name} {lower-case}OF{sentence-case} HER MAJESTY'S SPACE NAVY{lower-case}"
:TKN1_214 ""
:TKN1_215 "{left align} UNKNOWN PLANET"
:TKN1_216 "{clear screen}{tab 6}{move to row 10, white, lower case}{white}{all caps}INCOMING MESSAGE"
:TKN1_217 "CURRUTHERS"
:TKN1_218 "FOSDYKE SMYTHE"
:TKN1_219 "FORTESQUE"
:TKN1_220 "WAS LAST SEEN AT {single cap}REESDICE"
:TKN1_221 "IS BELIEVED TO HAVE JUMPED TO THIS GALAXY"
:TKN1_222 "{incoming message screen, wait 2s}{clear screen}{white}{tab 6, white, lower case in words}{justify}{sentence-case}GOOD DAY {single cap}COMMANDER{commander name}.\n{single cap}I{lower-case} AM {singlecap}AGENT{single cap}BLAKE OF {singlecap}NAVAL {single cap}INTELLEGENCE.\n{single cap}AS YOU KNOW, THE {singlecap}NAVY HAVE BEEN KEEPING THE {singlecap}THARGOIDS OFF YOUR ASS OUT IN DEEPSPACE FOR MANY YEARS NOW. {single cap}WELL THE SITUATION HAS CHANGED.\n{single cap}OUR BOYS ARE READY FOR A PUSH RIGHT TO THE HOME SYSTEM OF THOSEMOTHERS.\n{single cap}{wait for key press}{clear screen}{white}{tab 6, white, lower case in words}I{lower-case} HAVE OBTAINED THE DEFENCE PLANS FOR THEIR {single cap}HIVE{single cap}WORLDS.\n {single cap}THE BEETLES KNOW WE'VE GOT SOMETHING BUT NOT WHAT.\n {single cap}IF {singlecap}I TRANSMIT THE PLANS TO OUR BASE ON {single cap}BIRERA THEY'LL INTERCEPT THE TRANSMISSION. {single cap}I NEED A SHIP TO MAKE THE RUN.\n {single cap}YOU'RE ELECTED.\n {single cap}THE PLANS ARE UNIPULSE CODED WITHIN THIS TRANSMISSION.\n{single cap}{tab 6}YOU WILL BE PAID.\n{single cap}{single cap}GOOD LUCK {single cap}COMMANDER.\n{left align}{tab 6}{all caps}  MESSAGE ENDS{wait for key press}"
:TKN1_223 "{incoming message screen, wait 2s}{clear screen}{tab 6, white, lower case in words}{white}{tab 6}{justify}{lower-case}{single cap}WELL DONE  {single cap}COMMANDER.\n {single cap}YOU HAVE SERVED US WELL AND WE SHALL REMEMBER.\n{single cap}WE DID NOT EXPECT THE {single cap}THARGOIDS TO FIND OUT ABOUT YOU.\n{single cap}FOR THE MOMENT PLEASE ACCEPT THIS {single cap}NAVY {standard tokens, sentence case}EXTRA ENERGY UNIT{extended tokens} AS PAYMENT.\n{left align}{tab 6}{all caps}  MESSAGE ENDS{wait for key press}"
:TKN1_224 ""
:TKN1_225 "SHREW"
:TKN1_226 "BEAST"
:TKN1_227 "BISON"
:TKN1_228 "SNAKE"
:TKN1_229 "WOLF"
:TKN1_230 "LEOPARD"
:TKN1_231 "CAT"
:TKN1_232 "MONKEY"
:TKN1_233 "GOAT"
:TKN1_234 "FISH"
:TKN1_235 "<EQUB_71> <EQUB_66>"
:TKN1_236 "<GENR_system_adjective> <EQUB_225> <EQUB_240>"
:TKN1_237 "ITS <EQUB_76> <EQUB_230> <EQUB_240>"
:TKN1_238 "<EQUB_245> <EQUB_250>"
:TKN1_239 "<EQUB_71> <EQUB_66>"
:TKN1_240 "MEAT"
:TKN1_241 "CUTLET"
:TKN1_242 "STEAK"
:TKN1_243 "BURGERS"
:TKN1_244 "SOUP"
:TKN1_245 "ICE"
:TKN1_246 "MUD"
:TKN1_247 "ZERO-{single cap}G"
:TKN1_248 "VACUUM"
:TKN1_249 "<GENR_system_adjective> ULTRA"
:TKN1_250 "HOCKEY"
:TKN1_251 "CRICKET"
:TKN1_252 "KARATE"
:TKN1_253 "POLO"
:TKN1_254 "TENNIS"
:TKN1_255 ""
:EQUB_16  (equb-range 16) ; 0
:EQUB_21  (equb-range 21) ; 1
:EQUB_26  (equb-range 26) ; 2
:EQUB_31  (equb-range 31) ; 3
:EQUB_155  (equb-range 155) ; 4
:EQUB_160  (equb-range 160) ; 5
:EQUB_46  (equb-range 46) ; 6
:EQUB_165  (equb-range 165) ; 7
:EQUB_36  (equb-range 36) ; 8
:EQUB_41  (equb-range 41) ; 9
:EQUB_61  (equb-range 61) ; 10
:EQUB_51  (equb-range 51) ; 11
:EQUB_56  (equb-range 56) ; 12
:EQUB_170  (equb-range 170) ; 13
:EQUB_66  (equb-range 66) ; 14
:EQUB_71  (equb-range 71) ; 15
:EQUB_76  (equb-range 76) ; 16
:EQUB_81  (equb-range 81) ; 17
:EQUB_86  (equb-range 86) ; 18
:EQUB_140  (equb-range 140) ; 19
:EQUB_96  (equb-range 96) ; 20
:EQUB_101  (equb-range 101) ; 21
:EQUB_135  (equb-range 135) ; 22
:EQUB_130  (equb-range 130) ; 23
:EQUB_91  (equb-range 91) ; 24
:EQUB_106  (equb-range 106) ; 25
:EQUB_180  (equb-range 180) ; 26
:EQUB_185  (equb-range 185) ; 27
:EQUB_190  (equb-range 190) ; 28
:EQUB_225  (equb-range 225) ; 29
:EQUB_230  (equb-range 230) ; 30
:EQUB_235  (equb-range 235) ; 31
:EQUB_240  (equb-range 240) ; 32
:EQUB_245  (equb-range 245) ; 33
:EQUB_250  (equb-range 250) ; 34
:EQUB_115  (equb-range 115) ; 35
:EQUB_120  (equb-range 120) ; 36
 :EQUB_125  (equb-range 125) ; 37
 :sentence-case "" ;; just ignore the case specifications for now...
 :lower-case ""
 :single-cap ""
 :justify ""
 })

(<= 1 0)

(defn parse-goat-soup
  [unparsed parsed mode]
  ;;(println unparsed)
  (if (<= (count unparsed) 0)
    parsed
    (let [cursor (first unparsed)
          [add-to-parsed mode-change]
          (cond
            (= cursor "{")
            ["" [(inc (first mode)) "" (nth mode 2) ""]]
            (= cursor "}")
            ["" [(dec (first mode)) "" (nth mode 2) ""]]
            :else
            (if (<= (first mode) 0)
              (let [;;_ (println [unparsed parsed mode])
                    ;;_ (println mode (<= (first mode) 0))
                    ;;_ (println mode)
                    is-double-space (and (= " " cursor )(= " " (nth mode 3)))
                    cursor (if is-double-space "" cursor)
                    ]
                (cond (= (nth mode 2) :lower-case)
                      [(cstring/lower-case cursor) mode]
                      (= (nth mode 2) :sentence-case)
                      (let [;;_ (println mode " -> " (= "" (nth mode 3)))
                            ]
                        (if (or
                             (= " " (nth mode 3))
                             (= "" (nth mode 3)))
                          [(cstring/upper-case cursor) mode]
                          [(cstring/lower-case cursor) mode]))
                      (= (nth mode 2) :single-cap)
                      [(cstring/upper-case cursor) (assoc-in mode [2] :lower-case)]
                      :else
                      [cursor mode]))
              (let [accum (str (nth mode 1) cursor)                   
                    mode-switch
                    (cond
                      (= accum "lower-case")
                      :lower-case
                      (= accum "sentence-case")
                      :sentence-case
                      (or (= accum "single cap") (= accum "single-cap"))
                      (if (= :sentence-case (nth mode 2))
                        :sentence-case
                        :single-cap)                        
                      :else
                      :lower-case)
                    ;;_ (println mode-switch)
                    ]
                ["" [(nth mode 0) accum mode-switch cursor]
                 ])))
          ;;_ (println (str "<" add-to-parsed ">"))
          mode-change (assoc-in mode-change [3] add-to-parsed)]
      (parse-goat-soup (rest unparsed) (str parsed add-to-parsed) mode-change)
      )))

(parse-goat-soup "{single cap}tHIS {single cap}is a {sentence-case}string to parse {lower-case}PARSE." "" [0 "" nil ""])

(defn handler-fn [grammar rule-id [_ attribute value]]
  (assoc-in grammar [:data :model attribute] value))

(defn validator-fn [grammar rule-id [_ attribute value]]
  (= value (get-in grammar [:data :model attribute])))

(defn translate-grammar-rules
  [m f]
  (into (empty m) (for [[k v] m] [k (f v)])))

(def vowels #{\a \e \i \o \u})

(defn name-to-adjective [name]
  (let [base-name (if (some vowels (take-last 1 name)) (clojure.string/join "" (drop-last name)) name)]
    (clojure.string/join "" (concat base-name "ian"))))

;; (name-to-adjective "Lave")

;; (clojure.string/join "" (drop-last "test"))

;; (defn deterministic-shuffle
;;   [^java.util.Collection coll seed]
;;   (let [al (java.util.ArrayList. coll)
;;         rng (java.util.Random. seed)]
;;     (java.util.Collections/shuffle al rng)
;;     (clojure.lang.RT/vector (.toArray al))))

;; (defn make-determanistic-selector [seed]
;;   (fn [grammar head bodies]
;;     (let [;prng-hash (xxHash32. (js-str seed) (js-str bodies))
;;           ;;prng (random/create prng-hash)
;;           rnum ;(js/Uint8Array.)
;;                                         ;(clj->js)
;;                                         ;prng-hash
;;           (hash bodies)
;;           gen-count (get-in grammar [:generation-count] 0)
;;           uniseed (+ seed (hash bodies) (hash head) gen-count)
;;           myrng (rng/rng uniseed)
;;           ]
;;       ;;(.log js/console uniseed)
;;       ;;(println uniseed)
;;       ;;(println gen-count)
;;       ;;(println bodies)
;;       ;;(.log js/console rnum)
;;       ;;(.log js/console seed)
;;       ;;(.log js/console (+ rnum seed))
;;       ;;(.log js/console (determanistic-shuffle bodies (+ seed rnum)))
;;                                         ;(.log js/console (js-str seed))
;;       (-> grammar
;;           (update-in [:generation-count] inc)
;;           (assoc-in [:selected] ;;(first bodies)
;;                     (doall (first (rng/shuffle myrng bodies)))
;;                     )))))

;;; 
;; (hash ["ahwe"])

;; (defn obj->clj
;;   [obj]
;;   (if (goog.isObject obj)
;;     (-> (fn [result key]
;;           (let [v (goog.object/get obj key)]
;;             (if (= "function" (goog/typeOf v))
;;               result
;;               (assoc result key (obj->clj v)))))
;;         (reduce {} (.getKeys goog/object obj)))
;;     obj))

;; (js->clj (-> (xxHash32. "a" 0) js/JSON.stringify js/JSON.parse))
;; (obj->clj
;;  (xxHash32. "a" 0))
;; 16

;; ((make-determanistic-selector 9)
;;  {}
;;  []
;;  ["a" "b" "c"])

;; (defn determanistic-selector
;;   [grammar head bodies]
;;   (let [seed (get grammar :elite-seed 0)
;;         ;;prng (.-csprng/isaacCSPRNG seed
;;         prng-hash (xxHash32. seed "goat soup")
;;         prng (random/create prng-hash)
;;         ]
;;     (.log js/console grammar)
;;     (assoc-in grammar [:selected] ;;(first bodies)
;;               (doall (first (sample bodies :seed seed)))
;;                                          )))


;; (defn goat-soup [seed system-name]
;;   (let [determ (make-determanistic-selector seed)]
;;     (-> text-tokens-extended
;;         (assoc-in [:elite-seed] seed)
;;         (assoc-in [:system-name] system-name)
;;         (assoc-in [:system-adjective] (name-to-adjective system-name))
;;         (translate-grammar-rules #(if (string? %1) [%1] %1))
;;         (grot/create-grammar)
;;         (grot/set-handler :set handler-fn)
;;         (grot/set-validator :when validator-fn)
;;         (grot/set-selector determ)
;;         (grotesque.core/set-modifier :capitalize clojure.string/capitalize)
;;         (grot/generate "#TKN1_5#")
;;         ((fn [x] ;;[(keys x) (get-in x [:generated] nil) (get-in x [:errors] nil)]
;;            (if (empty? (get x :errors))
;;              (if (empty? (get x :generated))             
;;                (get-in x [:generated] "ERROR: missing generated result?")
;;                ;; ["EMPTY GENERATION"
;;                ;;  (keys x)
;;                ;;  (:errors x)
;;                ;;  (:selected x)
;;                ;;  (:generated x)
;;                ;;  ]
;;                (:generated x)
;;                )
;;              [(get-in x [:rules :TKN1_141]) [(get-in x [:errors] nil) (:rules x)]])
;;            )))))










;; (defn goat-soup-bbc [seed planet-name]
;;   (let [rand-seed []])

;;   )

(def elite-seed (utility/make-seed [0x5A4A 0x0248 0xB753]))
;;(goat-soup elite-seed "Tibedied")

;; (mapv #(get-seed-bits elite-seed % 0 8) [2 3 4 5])

;;  (get text-tokens-extended :TKN1_5)

;; (-> 
;;  (get text-tokens-extended :TKN1_5)
;;  #(cstring/split % #"\#")
;; ;\\
;;  )

;; (cstring/split (cstring/replace
;;                 (get text-tokens-extended :TKN1_5)
;;                 #"#([A-Za-z0-9_])"
;;                 "!$1!"
;;                 )
;;                #"")

;; (get text-tokens-extended :TKN1_5)

;; (defn goat-soup-invoke [word]
;;   ;;(goat-soup-recurse)
;;   (get text-tokens-extended (keyword word)))

;; (defn goat-soup-vary [word rand-seed]
;;   (get text-tokens-extended (keyword word))
;;   )



;; (byte-to-bin
;;  (get elite-seed ))

;; [elite-seed]
(print elite-seed)
(def goat-soup-seed
  (mapv #(utility/get-seed-bits elite-seed % 0 8) [2 3 4 5]))
(utility/goat-soup-next-rand goat-soup-seed)

;; (+ true true)

;; (apply + [1 1 ])

(defn capitalize-word [word]
  (str (cstring/upper-case (first word)) (cstring/lower-case (apply str (rest word)))))

(capitalize-word "udyidi")

(defn generate-random-word [rand-seed name-length name-exp]
  ;;(println "generate-random-word: " name-exp " = " name-length)
  (if (> 0 name-length)
    [(str "{single cap}" (capitalize-word (cstring/join "" [name-exp]))) rand-seed]
    (let [[rnd-choice next-seed] (utility/goat-soup-next-rand rand-seed)
          word-choice
          ;; (/(utility/bin-to-byte
          ;;                (mapv *
          ;;                      (utility/byte-to-bin rnd-choice)
          ;;                      [0 0 1 1 1 1 1 0]))
          ;;               2)
          (/ (bit-and rnd-choice 0x3e) 2)
          ;;_ (println (str "word-choice: " word-choice))
          chosen-token (get two-letter-tokens (+ 216 word-choice) [0 "**"])
          ;;_ (println chosen-token)
          name-exp (cstring/join "" [name-exp chosen-token])
          [recurse-expand next-seed] (generate-random-word next-seed (- name-length 1) name-exp)
          ]
      [ recurse-expand next-seed])))

;; (let [a 35]
;;   (generate-random-word [(utility/byte-to-bin a)
;;                          (utility/byte-to-bin a)
;;                          (utility/byte-to-bin a)
;;                          (utility/byte-to-bin a)
;;                          ]
;;                         4
;;                         ""))

;; (utility/bytes-to-seed)
;; (sort
;;  (set
;;   (mapv (fn [a] (first
;;                  (generate-random-word [(utility/byte-to-bin a)
;;                                         (utility/byte-to-bin a)
;;                                         (utility/byte-to-bin a)
;;                                         (utility/byte-to-bin a)
;;                                         ]
;;                                        4 "")
;;                  )
          
;;           )
;;         (range 256))))


;; (count two-letter-tokens)
;; (count "ABOUSEITILETSTONLONUTHNOALLEXEGEZACEBISOUSESARMAINDIREA.ERATENBERALAVETIEDORQUANTEISRION")


;; (defn next-expand [tokens rand-seed planet-name]
;;   (println "(next-expand " tokens ")")
;;   ;;(mapv (fn [x] (print x "+")) tokens)
;;   (let [variances-remaining (count (filter #(or (cstring/starts-with? % "EQUB")
;;                                                 (cstring/starts-with? % "TKN1")
;;                                                 (cstring/starts-with? % "GENR"))
;;                                            tokens))]
;;     (if (< 0 variances-remaining)
;;       (let [first-variance (some #(when (or
;;                                          (cstring/starts-with? % "TKN1")
;;                                          (cstring/starts-with? % "EQUB")
;;                                          (cstring/starts-with? % "GENR")) %) tokens)
;;             variance-indexes (.indexOf tokens first-variance)]
;;         (if first-variance
;;           (cond (cstring/starts-with? first-variance "TKN1")
;;                 [tokens rand-seed]
;;                 (cstring/starts-with? first-variance "EQUB")
;;                 (let [_ (println first-variance)
;;                       [rnd-choice next-seed] (utility/goat-soup-next-rand rand-seed)
;;                       _ (println "\tnext-seed: " next-seed)
;;                       _ (println "\t int-seed: " (mapv utility/bin-to-byte next-seed))
;;                       choice-index (apply + (mapv #(>= rnd-choice %) [0x33 0x66 0x99 0xCC]))
;;                       choices (get text-tokens-extended (keyword first-variance))
;;                       new-token (nth choices choice-index)
;;                       new-token-list (assoc tokens variance-indexes new-token)]
;;                   (println (str rnd-choice) " = " (str choice-index) " -> " new-token)
;;                  [(cstring/join "#" new-token-list) next-seed])
                
;;                 (cstring/starts-with? first-variance "GENR")
;;                 (let [[new-token new-seed]
;;                       (cond
;;                         (cstring/starts-with? first-variance "GENR_system_name")
;;                         [(str planet-name) rand-seed]
;;                         (cstring/starts-with? first-variance "GENR_system_adjective")
;;                         [(str planet-name "ian") rand-seed]
;;                         (cstring/starts-with? first-variance "GENR_8")
;;                         (let [[rnd-choice next-seed] (utility/goat-soup-next-rand rand-seed)
;;                               name-length (utility/bin-to-byte
;;                                            (mapv * (utility/byte-to-bin rnd-choice) [0 0 0 0 0 0 1 1]))
;;                               [gen-word next-seed] (generate-random-word next-seed name-length "")]
;;                           [gen-word next-seed]                          )
;;                         :else
;;                         ["<GENERATED WORD>" rand-seed])]
;;                   [(cstring/join "#"
;;                                  (assoc tokens variance-indexes new-token))
;;                    new-seed])                  
;;                 :else
;;                 (let [new-token "<UNKNOWN>"
;;                       new-token-list (assoc tokens variance-indexes new-token)]
;;                   [(cstring/join "#" new-token-list) rand-seed])
;;                 )
;;           [tokens rand-seed] ))
;;       [tokens rand-seed] )))

;; (defn goat-soup-split-terms [tokens]
;;   (if (string? tokens)
;;     (cstring/split (cstring/replace
;;                     (cstring/replace tokens
;;                                      #"#([A-Za-z0-9_]*?)#"
;;                                      "#$1#") "##" "#") "#")
;;     (goat-soup-split-terms
;;      (cstring/join "#" tokens))))

;; ;;(goat-soup-split-terms "#EQUB_86# more text #TKN1_140# #GENR_8#")



;; (defn goat-soup-parse-expand [data-start data-seed planet-name])

;; (defn goat-soup-recurse [data-start data-seed planet-name]
;;   (println "(goat-soup-recurse " data-start ")")
;;   (let [;;data-seed goat-soup-seed
;;         ;;data-start "#EQUB_86# more text #TKN1_140#"
;;         _ (println data-seed)
;;         data-start (goat-soup-split-terms data-start)
;;         pending-replacements (filter (fn [word] (or
;;                                                  (cstring/starts-with? word "TKN1")
;;                                                  (cstring/starts-with? word "GENR")
;;                                                  (cstring/starts-with? word "EQUB")))
;;                                      data-start)
;;         ;;_ (println pending-replacements)
;;         data-start (if (= 0 (count pending-replacements))
;;                      (cstring/join data-start)
;;                      data-start
;;                      )
;;         data-split (goat-soup-split-terms data-start)
;;         _ (println data-split)
;;         finished (= 1 (count data-split))
;;         ]
;;     ;;(println data-split)
;;     ;;(println finished)
;;     (if finished
;;       data-split
;;       ;; (if (string? data-split)
;;       ;;   data-split
;;       ;;   (first data-split))
;;       (let [data-expanded (mapv (fn [word]
;;                                  (if (cstring/starts-with? word "TKN1")
;;                                    (get text-tokens-extended (keyword word))
;;                                    word))
;;                                data-split)
;;             [new-data new-seed] (next-expand data-expanded data-seed planet-name)]
;;         ;;(println [new-data new-seed])
;;         (goat-soup-recurse new-data new-seed planet-name)
;;         ;;[new-data new-seed]
;;         ))))



;; (->
;;  (goat-soup-recurse "#EQUB_86# more text #TKN1_140#" goat-soup-seed)
;;  (fn [a] (goat-soup-recurse (first a) (second a)))
;;  )

;; (goat-soup-split-terms
;;  (goat-soup-split-terms "no splits left#TKN1_140#"))

;; (defn generate-goat-soup [planet-seed planet-name]
;;   (parse-goat-soup
;;    (let [planet-goat-soup-seed
;;          (mapv #(utility/get-seed-bits planet-seed % 0 8) [2 3 4 5])]
;;      (println (str "seed: " planet-goat-soup-seed))
;;      (cstring/join ""
;;                    (goat-soup-recurse "#TKN1_5#" planet-goat-soup-seed planet-name)))
;;    "" [0 "" :lower-case ""]))

;; (let [x (generate-goat-soup elite-seed "TIBEDIED")]
;;   (println x)
;;   x)

;;      "This planet is most fabled for Its Exciting Leopard Meat but scourged by deadly mountain bienceoids.\n"
;;      "This planet is most notable for Tibediedian Arnu brandy but ravaged by unpredictable solar activity."]

;; (let [[d s] (goat-soup-recurse "#EQUB_86# more text #TKN1_140#" goat-soup-seed)
;;       [d s] (goat-soup-recurse d s)
;;       [d s] (goat-soup-recurse d s)
;;       [d s] (goat-soup-recurse d s)
;;       [d s] (goat-soup-recurse d s)
;;       [d s] (goat-soup-recurse d s)
;;       [d s] (goat-soup-recurse d s)
;;       [d s] (goat-soup-recurse d s)
;;             [d s] (goat-soup-recurse d s)
;;       ]
;;   (goat-soup-recurse d s)
;;   )



















;; (defn goat-soup-recurse [tokens rand-seed]
;;   (let [split-tokens (cond (string? tokens)
;;                            (flatten (goat-soup-split-terms tokens))
;;                            (vector? tokens)
;;                            (mapv goat-soup-split-terms (flatten tokens))
;;                            :else [:c tokens])
;;         my-tokens
;;         (mapv (fn [word]
;;                 (cond
;;                   (cstring/starts-with? word "TKN1") (goat-soup-invoke word)
;;                   ;;(cstring/starts-with? word "EQUB") (goat-soup-vary word rand-seed)
;;                   :else word))
;;               split-tokens)]
;;     (println my-tokens)
;;     (next-expand
;;      my-tokens
;;      rand-seed)))

;; (goat-soup-recurse (get text-tokens-extended :TKN1_5) goat-soup-seed)


;; (get text-tokens-extended :TKN1_5)




                                        ;(doc sample)
                                        ;(keys)
;; (goat-soup 5090 "Lave")
;; (:TKN1_5 (:rules (goat-soup 0 "Lave")))


;;(sample (range 5) :seed 7)


;;(goat-soup 1 "Test")


;; (println
;;  (frequencies 
;;   (map #(goat-soup % "Lave") (range 200))))


(defn resolve-token [token seed planet-name]
  ;;(println (str "resolve-token: " token "\t\t" (mapv utility/bin-to-byte seed)))
  (cond
    (cstring/starts-with? token "TKN1")
    ["" (get text-tokens-extended (keyword token)) seed]
    (cstring/starts-with? token "EQUB")
    (let [[rnd-choice next-seed] (utility/goat-soup-next-rand seed)
          choice-index (apply + (mapv #(>= rnd-choice %) [0x33 0x66 0x99 0xCC]))
          choices (get text-tokens-extended (keyword token))
          new-token (nth choices choice-index)
          ;;_ (println (str "chosing new token: " rnd-choice " > " choice-index " = " new-token))
          ]
      ["" new-token next-seed])
    (cstring/starts-with? token "GENR")
    (let [[new-token new-seed]
          (cond
            (cstring/starts-with? token "GENR_system_name")
            [(str "{single cap}" (capitalize-word (str planet-name))) seed]
            (cstring/starts-with? token "GENR_system_adjective")
            [(str "{single cap}" (capitalize-word (str planet-name "ian"))) seed]
            (cstring/starts-with? token "GENR_8")
            (let [[rnd-choice next-seed] (utility/goat-soup-next-rand seed)
                  ;;_ (println (str "rnd-choice: " rnd-choice))
                  name-length
                  (bit-and rnd-choice 3)
                  ;; (utility/bin-to-byte
                  ;;  (mapv * (utility/byte-to-bin rnd-choice) [0 0 0 0 0 0 1 1]))
                  ;;_ (println (str "name-length: " name-length))
                  [gen-word next-seed] (generate-random-word next-seed name-length "")]
              [gen-word next-seed])            
            :else
            ["" seed])
          ]      
      ["" new-token new-seed])
    :else
    [(str "(unknown token: " token ")") "" seed]))

(defn parse-expand-grammar [unparsed parsed seed planet-name mode]
  ;;(println (str parsed "<-:" unparsed ":" mode))
  (if (< 955 (nth mode 2))
    [unparsed parsed]    
    (if (<= (count unparsed) 0)
      parsed
      (let [cursor (first unparsed)
            [add-to-parsed add-to-unparsed new-seed updated-mode]
            (cond
              (= (first mode) :token)
              (cond
                (= cursor ">")
                (let [[resolved-token resolved-unparsed resolved-seed]
                      (resolve-token (nth mode 1) seed planet-name)]
                  [resolved-token
                   resolved-unparsed
                   resolved-seed
                   (assoc-in mode [0] :text)])
                :else
                ["" "" seed (update-in mode [1] #(str % cursor))]
                )
              (= (first mode) :text)
              (cond
                (= cursor "<")
                ["" "" seed (assoc-in
                             (assoc-in mode [0] :token)
                             [1]
                             "")]
                :else
                [cursor "" seed mode]
                )
              :else
              [cursor "" seed mode]
              )
            ]
        (parse-expand-grammar (str add-to-unparsed (apply str (rest unparsed)))
                              (str parsed add-to-parsed)
                              new-seed
                              planet-name
                              (update-in updated-mode [2] inc))))))

(defn generate-goat-soup [planet-seed planet-name]
  (let [planet-goat-soup-seed (mapv #(utility/get-seed-bits planet-seed % 0 8) [2 3 4 5])
        expand (parse-expand-grammar "<TKN1_5>" "" planet-goat-soup-seed planet-name [:text "" 0])
        result (parse-goat-soup expand "" [0 "" :lower-case ""])
        ]
    result)
  ;; (parse-goat-soup
  ;;  (let [planet-goat-soup-seed
  ;;        (mapv #(utility/get-seed-bits planet-seed % 0 8) [2 3 4 5])]
  ;;    (println (str "seed: " planet-goat-soup-seed))
  ;;    (cstring/join ""
  ;;                  (goat-soup-recurse "#TKN1_5#" planet-goat-soup-seed planet-name)))
  ;;  "" [0 "" :lower-case ""])
  )

(str "X" (apply str (rest "test")))

;; (let [x (generate-goat-soup elite-seed "TIBEDIED")]
;;   (println x)
;;   x)

;; (let [x (generate-goat-soup (utility/twist-to-next-planet elite-seed) "TIBEDIED")]
;;   (println x)
;;   x)

(let [x (generate-goat-soup
         (utility/twist-to-next-planet
          (utility/twist-to-next-planet
           (utility/twist-to-next-planet
            (utility/twist-to-next-planet
             elite-seed)))) "TIBEDIED")]
  (println x)
  x)

(println "grammar")
