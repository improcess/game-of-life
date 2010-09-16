(ns game-of-life.core
  (:require [polynome.core :as poly]))

(def m (poly/init "/dev/tty.usbserial-m64-0790"))

(def state (atom (zipmap (poly/coords m) (repeat 0))))

(poly/clear m)

(defn draw-state
  []
  (apply poly/frame m (partition 8 (let [s @state]
                                     (map #(get s %) (poly/coords m))))))



(poly/frame m [1 1 1 1 1 1 1 1]
              [1 1 1 1 1 1 1 1]
              [1 1 1 1 1 1 0 1]
              [1 1 1 1 1 1 1 1]
              [1 1 1 1 1 1 1 1]
              [1 1 0 1 1 1 1 1]
              [1 1 1 1 1 0 1 1]
              [1 1 1 1 1 1 1 1])

(poly/frame m [1 1 1 1 1 1 1]
            [0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0])

(poly/on-press m (fn [x y] (swap! state assoc [x y] 1)))
