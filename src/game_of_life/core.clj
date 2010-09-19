(ns game-of-life.core
  (:use
   overtone.live)
  (:require [polynome.core :as poly]
            [overtone-contrib.core :as contrib]))

(def m (poly/init "/dev/tty.usbserial-m64-0790"))

(def state (atom (zipmap (poly/coords m) (repeat 0))))

(defn draw-state
  ([] (apply poly/frame m (poly/map->frame m @state)))
  ([time]
     (apply poly/frame-at m time (poly/map->frame m @state))))

(defn num-alive-neighbours
  [coords world]
  (let [x (first coords)
        y (second coords)
        nw (get world [(dec x) (dec y)] 0)
        n  (get world [x       (dec y)] 0)
        ne (get world [(inc x) (dec y)] 0)
        w  (get world [(dec x) y]       0)
        e  (get world [(inc x) y]       0)
        sw (get world [(dec x) (inc y)] 0)
        s  (get world [x       (inc y)] 0)
        se (get world [(inc x) (inc y)] 0)]
    (+ ne n nw w e sw s se)))

(defn spawn?
  [alive-neighbour-count]
  (= alive-neighbour-count 3))

(defn continue-living?
  [alive-neighbour-count]
  (or (= alive-neighbour-count 2)
      (= alive-neighbour-count 3)))

(defn lives?
  [coords world]
  (let [currently-alive?      (= 1 (get world coords))
        alive-neighbour-count (num-alive-neighbours coords world)]
    (if currently-alive?
      (continue-living? alive-neighbour-count)
      (spawn? alive-neighbour-count))))

(defn evolve
  [state]
  (into {} (map #(vec (list % (if (lives? % state) 1 0))) (keys state))))

(defn update-state
  []
  (swap! state evolve))

(def metro (metronome 880))

(defn go [beat]
  (let [time (metro beat)]
    (draw-state-at time)
    (update-state)
    (contrib/apply-before #'go (metro (inc beat)) (inc beat))))

(go (metro))

(poly/on-release m (fn [x y] (swap! state assoc [x y] 1)))


