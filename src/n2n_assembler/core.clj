(ns n2n-assembler.core
  (:require [clojure.string :as str]
            [clojure.pprint :refer (cl-format)])
  (:gen-class))

(defn remove-comment
  [code]
  (if (str/includes? code "//")
    (.substring code 0 (str/index-of code "//"))
    code))

(defn remove-whitespace
  [codes]
  (-> codes
      (str/replace " " "")
      (str/replace "/t" "")))

(defn comment? [code]
  (str/starts-with? code "//"))

(defn a-instruction? [code]
  (str/starts-with? code "@"))

(defn number-to-a-inst
  [n]
  (cl-format nil "0~15,'0',B" n))

(defn trans-a-instruction
  [code symbols symbol-idx]
  (let [code (.substring code 1 (count code))]
    (number-to-a-inst
      (cond
        (number? (read-string code)) (read-string code)
        (contains? @symbols code)    (get @symbols code)
        :else                        (do
                                       (swap! symbols assoc code @symbol-idx)
                                       (swap! symbol-idx inc)
                                       (get @symbols code))))))

(defn reference? [code]
  (str/starts-with? code "("))

(defn code-type [code]
  (cond
    (reference? code)     :reference
    (a-instruction? code) :a-instruction
    :else                 :c-instruction))

(defn parse-codes
  [codes]
  (->> codes
       str/split-lines
       (map remove-comment)
       (map remove-whitespace)
       (filter #(not (str/blank? %)))))

(def predefined-symbols
  {"SP"     0
   "LCL"    1
   "ARG"    2
   "THIS"   3
   "THAT"   4
   "R0"     0
   "R1"     1
   "R2"     2
   "R3"     4
   "R4"     4
   "R5"     5
   "R6"     6
   "R7"     7
   "R8"     8
   "R9"     9
   "R10"    10
   "R11"    11
   "R12"    12
   "R13"    13
   "R14"    14
   "R15"    15
   "SCREEN" 16384
   "KBD"    24576})

(def c-comp-fields
  {"0"   "101010"
   "1"   "111111"
   "-1"  "111010"
   "D"   "001100"
   "A"   "110000"
   "M"   "110000"
   "!D"  "001101"
   "!A"  "110001"
   "!M"  "110001"
   "-D"  "001111"
   "-A"  "110011"
   "-M"  "110011"
   "D+1" "011111"
   "A+1" "110111"
   "M+1" "110111"
   "D-1" "001110"
   "A-1" "110010"
   "M-1" "110010"
   "D+A" "000010"
   "D+M" "000010"
   "D-A" "010011"
   "D-M" "010011"
   "A-D" "000111"
   "M-D" "000111"
   "D&A" "000000"
   "D&M" "000000"
   "D|A" "010101"
   "D|M" "010101"})

(def c-dest-fields
  {nil   "000"
   "M"   "001"
   "D"   "010"
   "MD"  "011"
   "A"   "100"
   "AM"  "101"
   "AD"  "110"
   "AMD" "111"})

(def c-jump-fields
  {nil   "000"
   "JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLT" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(defn trans-c-instruction
  [code]
  (let [jump-field   (second (str/split code #";"))
        dest-comp    (str/split (first (str/split code #";")) #"=")
        is-comp-only (= (count dest-comp) 1)
        dest-field   (if is-comp-only nil (first dest-comp))
        comp-field   (if is-comp-only (first dest-comp) (second dest-comp))]
    (str
      "111"
      (if (str/includes? (or comp-field "") "M") "1" "0")
      (get c-comp-fields comp-field)
      (get c-dest-fields dest-field)
      (get c-jump-fields jump-field))))

(defn reference-line-to-keyword
  [code]
  (-> code
      (str/replace "(" "")
      (str/replace ")" "")))

(defn label-symbol-mapping
  [codes]
  (into {}
        (->> codes
             (map-indexed vector)
             (filter #(reference? (second %)))
             (map (fn [[idx itm]]
                    [(reference-line-to-keyword itm) idx]))
             (map-indexed vector)
             (map (fn [[offset [itm itm-idx]]]
                    [itm (- itm-idx offset)])))))

(label-symbol-mapping (parse-codes (slurp "resources/max/Max.asm")))

(defn assembler
  [source]
  (let [codes         (parse-codes source)
        label-symbols (label-symbol-mapping codes)
        symbols       (atom (merge predefined-symbols label-symbols))
        symbol-idx    (atom 16)]
    (->> codes
         (map
           (fn [code]
             (cond (a-instruction? code) (trans-a-instruction code symbols symbol-idx)
                   (reference? code)     nil
                   :else                 (trans-c-instruction code))))
         (remove nil?))))

(defn -main
  [& args]
  (let [input-path   (first args)
        output-path  (second args)
        input        (slurp input-path)
        output-lines (assembler input)
        output       (str (str/join "\n"  output-lines) "\n")]
    (spit  output-path output)))
