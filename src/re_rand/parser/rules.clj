;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; re-rand.parser.rules:
;;
;; Rules to parse a regular expression into a series of string generating
;; functions.

(ns re-rand.parser.rules
  (:require [clojure.set :refer [difference]]
            [clojure.test.check.generators :as gen]
            [re-rand.parser.tools :refer :all]))

(defn parse-int
  [n]
  (Integer/parseInt n))

(defn char-range
  [from to]
  (map char
    (range (int (first from))
           (inc (int (first to))))))

(def digits
  (char-range "0" "9"))

(def whitespace
  " \t\n\f\r")

(def alphanumeric
  (concat
    (char-range "A" "Z")
    (char-range "a" "z")
    digits))

(def valid-any-chars
  (concat
    alphanumeric
    "_-/+*=%()[]{}!?:;,. \t\n"))

(defn invert
  "Return a set of characters that do not contain any of chars."
  [chars]
  (difference (set valid-any-chars) (set chars)))

(defn first-if-single
  "If a collection has only one item, return that item."
  [coll]
  (if (next coll)
    coll
    (first coll)))

(defn combine-groups
  "Combine tokens into groups, using a function to determine how to merge two
  adjacent groups."
  [merge-func tokens]
  (reduce
    (fn [groups token]
      (if (vector? token)
        (apply vector
          (str (groups 0) (token 0))
          (merge-func (subvec groups 1) (subvec token 1)))
        (assoc groups 0
          (str (groups 0) token))))
    [""]
    tokens))

(def repeat-limit 20)

(def escaped
  (attach
    (match #"\\(.)")
    (fn [[_ char]]
      (cond
        (= char "d") (gen/elements digits)
        (= char "s") (gen/elements whitespace)
        (= char "w") (gen/elements alphanumeric)
        (= char "D") (gen/elements (invert digits))
        (= char "S") (gen/elements (invert whitespace))
        (= char "W") (gen/elements (invert alphanumeric))
        :otherwise   (gen/return char)))))

(def literal
  (attach
    (match #"[^\\{}.+*()\[\]^$]")
    gen/return))

(def any-char
  (attach
    (match #"\.")
    (fn [_] (gen/elements valid-any-chars))))

(defn sequence-of-chars
  [src]
  (let [f (match #"((\\.|[^\^\-\[\]\\])+)([^-]|$)")]
    (if-let [[[_ m _ s] src] (f src)]
      [(.replace m "\\" "")
       (str s src)])))

(def range-of-chars
  (attach
    (match #"(\\.|[^\^\-\[\]\\])-(\\.|[^\^\-\[\]\\])")
    (fn [[_ from to]] (char-range from to))))

(defn get-char-list
  [char-groups invert?]
  (let [chars (apply concat char-groups)]
    (if invert?
      (invert chars)
      chars)))

(def char-class
  (attach
    (series
      (match #"\[")
      (match #"\^?")
      (many (choice sequence-of-chars range-of-chars))
      (match #"\]"))
    (fn [[_ invert? char-groups _]]
      (let [chars (get-char-list char-groups (seq invert?))]
        (gen/elements chars)))))

(declare pattern)

(def sub-pattern
  (attach
    (series
      (match #"\(")
      (forward pattern)
      (match #"\)"))
    (fn [[_ f _]] f)))

(def single
  (choice escaped
          sub-pattern
          any-char
          char-class
          literal))

(defn combine-many
  [tokens]
  (first-if-single (combine-groups (fn [x y] y) tokens)))

(def zero-or-more
  (attach
    (series single (match #"\*"))
    (fn [[f _]]
      (gen/fmap #(apply str %) (gen/list f)))))

(def one-or-more
  (attach
   (series single (match #"\+"))
   (fn [[f _]]
     (gen/fmap #(apply str %)
               (gen/such-that not-empty (gen/list f))))))

(def zero-or-one
  (attach
    (series single (match #"\?"))
    (fn [[f _]] (gen/fmap #(apply str %) (gen/vector f 0 1)))))

(def exactly-n
  (attach
    (series single (match #"\{(\d+)\}"))
    (fn [[f [_ n]]] (gen/fmap #(apply str %) (gen/vector f n)))))

(def between-n-and-m
  (attach
    (series single (match #"\{(\d+),\s*(\d+)\}"))
    (fn [[f [_ n m]]] (gen/fmap #(apply str %) (gen/vector f n m)))))

(def pipe
  (attach
   (series single (match #"\|") single)
   (fn [[left _pipe right]]
     (gen/one-of [left right]))))

(def pattern
  (attach
    (many
      (choice zero-or-more
              one-or-more
              zero-or-one
              exactly-n
              between-n-and-m
              pipe
              single))
    (fn [fs]
      (gen/fmap #(apply str %) (apply gen/tuple fs)))))
