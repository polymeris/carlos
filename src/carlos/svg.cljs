(ns carlos.svg
  (:require [goog.string :as s]
            [goog.string.format]
            [rand-cljc.core :as rng]))

(defn pattern-id [palette]
  (apply str (flatten palette)))

(defn pattern-ref [palette]
  (str "url(#" (pattern-id palette) ")"))

(defn pattern [config palette]
  (let [h (:height config)]
    [:pattern
     {:id            (pattern-id palette)
      :width         (count palette)
      :y             (/ h -2)
      :height        h
      :pattern-units :userSpaceOnUse}
     (map (fn [i [c t]]
            [:rect {:transform (s/format "rotate(%f)" t)
                    :x         i :y 0 :width 1.4 :height h
                    :fill      c
                    :key       (str (pattern-id palette) i)}])
          (iterate inc 0)
          palette)]))

(defn rectangle [w h pattern]
  [:rect {:x (/ w -2) :y (/ h -2) :width w :height h :fill pattern}])

(defn circle [diameter pattern]
  [:circle {:r    (/ diameter 2)
            :fill pattern}])

(defn ring [outer-diameter inner-diameter pattern]
  (let [outer-radius (/ outer-diameter 2)
        inner-radius (/ inner-diameter 2)]
    [:circle {:r            outer-radius
              :stroke-width (- outer-radius inner-radius)
              :fill         :none
              :stroke       pattern}]))

(defn diamond [w h pattern]
  (let [dx (/ w 2)
        dy (/ h 2)]
    [:polygon {:points (str "0,-" dy " "
                            dx ",0 "
                            "0," dy " "
                            "-" dx ",0")
               :fill   pattern}]))

(defn shape [config]
  [:g {:key       (s/format "shape-%d" (:shift-shape config))
       :transform (s/format "translate(%d)" (:shift-shape config))}
   (case (:shape config)
     :rectangle [rectangle (:shape-width config) (:shape-height config) (pattern-ref (:palette config))]
     :circle [circle (:shape-diameter config) (pattern-ref (:palette config))]
     :ring [ring (:shape-diameter config) (:shape-inner-diameter config) (pattern-ref (:palette config))]
     :diamond [diamond (:shape-width config) (:shape-height config) (pattern-ref (:palette config))])])

(defn random-palette [n prng]
  (map #(do [% (/ (dec (rng/rand-int prng 6)) 4)])
       (conj (take n (rng/shuffle prng ["black" "red" "green" "yellow" "blue"]))
             "transparent")))

(defn background-palette [p] p
  (conj (take (dec (count p)) p)
        ["black" 0]))

(defn parse-config [config]
  (let [seed (or (:seed config) (rand-int 1e7))
        prng (rng/rng seed)
        prng-int (fn [min max] (+ min (rng/rand-int prng (- max min))))
        prng-nth (partial rng/rand-nth prng)
        complexity (or (:complexity config) 10)
        width (or (:width config) (* 25 complexity))
        height (or (:height config) (* 25 complexity))
        max-shapes (or (:max-shapes config) (Math/floor (Math/sqrt complexity)))
        num-colors (or (:num-colors config) (if (> complexity 3) 4 3))
        palette (or (:palette config) (random-palette num-colors prng))]
    (-> config
        (assoc :width width)
        (assoc :height height)
        (update :shape #(or % (prng-nth [:rectangle :circle :ring :diamond])))
        (assoc :max-shapes max-shapes)
        (update :num-shapes #(or % (- max-shapes (prng-int 0 (/ max-shapes 2)))))
        (update :shape-distribution #(or % (prng-nth [:linear :centered])))
        (update :shape-width #(or % (prng-int (/ width 2) width)))
        (update :shape-height #(or % (prng-int (/ height 2) height)))
        (update :shape-diameter #(or % (prng-int (/ height 2) height)))
        (update :shape-inner-diameter #(or % (prng-int (/ height 4) (/ height 2))))
        (update :phase #(or % (prng-int 0 360)))
        (update :shift #(or % (* 6 (prng-int (/ complexity 2) complexity))))
        (assoc :num-colors num-colors)
        (assoc :palette palette)
        (update :background-palette #(or % (background-palette palette))))))

(defn palettes [config]
  [:defs
   [pattern config (:background-palette config)]
   [pattern config (:palette config)]])

(defn background [config]
  (let [w (:width config)
        h (:height config)]
    [:rect {:id   :background
            :x    (/ w -2) :y (/ h -2) :width w :height h
            :fill (pattern-ref (:background-palette config))}]))

(defn foreground [config]
  (let [n (:num-shapes config)
        s (:shift config)]
    [:g {:id      :foreground
         :opacity 0.5}
     (doall (map
              #(shape (assoc config :shift-shape (* s (- % (/ (dec n) 2)))))
              (range n)))
     (when (:animation config)
       [:animateMotion {:dur         45
                        :repeatCount :indefinite}
        [:mpath {:xlink-href :#motion-path}]])]))

(defn illustration [config]
  (let [config (parse-config config)
        w (:width config)
        h (:height config)
        s (:shift config)]
    (println "Using configuration" config)
    [:svg
     {:view-box (str (/ w -2) " "
                     (/ h -2) " "
                     w " " h)}
     [:path {:id :motion-path
             :d  (s/format "M0 0 H-%d H%d H0" (/ s 2) (/ s 2))}]
     [palettes config]
     [background config]
     [foreground config]]))