(ns game-of-life.core
  (:use [overtone.live]
        [overtone.device.grid])
  (:require [polynome.core :as poly]
            [overtone.at-at :as at]))

;;(def m (poly/init "/dev/tty.usbserial-m64-0790"))
(def m (poly/init "/dev/tty.usbserial-m256-203"))
(comment
  (poly/on-press m ::toggle (fn [x y s] (poly/toggle-led m x y)))
  (poly/all m)
  (poly/disconnect m)
  (time (draw-state)))

(def state (atom (zipmap (poly/coords m) (repeat 0))))

(defn draw-state
  []
  (dorun
   (map (fn [[[x y] led-val]]
          (poly/led m x y led-val))
        @state)))

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

(def metro (metronome 250))

(defn go [beat]
  (let [time (metro beat)]
    (at/at time #(draw-state))
;;    (println "hi")
    (update-state)
    (apply-at (metro (inc beat)) #'go [(inc beat)])))

(go (metro))
(poly/on-press m ::start-life (fn [x y s]
                                (poly/led-on m x y)
                                (swap! state assoc [x y] 1)))

(comment
  (get @state [4 0])
  (poly/on-press m ::beep (fn [x y s] (demo 0.2 (sin-osc))))
  (swap! state assoc [1 1] 1 [1 2] 1)
  (poly/on-press m ::debug (fn [x y s] (println "Pressed: " x y)))
  (poly/on-press m ::light (fn [x y s] (poly/led-on m x y)))
  (poly/on-release m ::dark (fn [x y s] (poly/led-off m x y)))

  (poly/led-off m 1 1)
  (poly/led-on m 1 1)

  (poly/disconnect m)

  (poly/remove-all-callbacks m)
  (stop))
(count (:queue m))

(dotimes [x 100]

  (do
    (apply poly/single-frame m  0 f2 )
    (apply poly/single-frame m  1 f2 )
    (apply poly/single-frame m  2 f2 )
    (apply poly/single-frame m  3 f2 ))
  (Thread/sleep 200)
  (do
    (apply poly/single-frame m  0 f )
    (apply poly/single-frame m  1 f )
    (apply poly/single-frame m  2 f )
    (apply poly/single-frame m  3 f ))
  (Thread/sleep 200))
