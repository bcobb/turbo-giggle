(ns internet-monitoring.core
  (:require [clojure.string :as str]))

(def votes
  [:tn
   :wy
   :mo
   :ar
   :nc
   :wv
   :la
   :ms
   :me
   :tn
   :tx
   :ar
   :id
   :tx
   :mt
   :wy
   :ia
   :ne
   :az
   :co
   :sc
   :ia
   :ut
   :nv
   :nd
   :ok
   :wi
   :la
   :ok
   :ut
   :az
   :ky
   :ks
   :ak
   :ga
   :oh
   :id
   :ks
   :sd
   :fl
   :ne
   :sc
   :al
   :al
   :ak
   :sd
   :nc
   :pa
   :ms
   :in])

(def states
  #{:al
    :ak
    :az
    :ar
    :ca
    :co
    :ct
    :de
    :fl
    :ga
    :hi
    :id
    :il
    :in
    :ia
    :ks
    :ky
    :la
    :me
    :md
    :ma
    :mi
    :mn
    :ms
    :mo
    :mt
    :ne
    :nv
    :nh
    :nj
    :nm
    :ny
    :nc
    :nd
    :oh
    :ok
    :or
    :pa
    :ri
    :sc
    :sd
    :tn
    :tx
    :ut
    :vt
    :va
    :wa
    :wv
    :wi
    :wy})

(defn state->color [state]
  (let [intensity (count (filter #(= % state) votes))]
    (condp = intensity
      0 "white"
      1 "salmon"
      2 "crimson")))

(defn build-css []
  (for [state (sort-by name states)]
    (let [color (state->color state)
          selector-name (str/upper-case (name state))]
      (str "#" selector-name " { fill: " color  " }"))))

(defn valid-state? [state]
  (contains? states state))

(defn generate-assets []
  (let [valid-premises (every? valid-state? votes)]
    (if valid-premises
      (let [css (str/join "\n" (build-css))]
        (spit "states.css" css))
      (let [invalid-votes (remove valid-state? votes)]
        (str "These are not valid votes: " (str/join ", " invalid-votes))))))
