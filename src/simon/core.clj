(ns simon.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.timbre :as log]
            [clojure.math.combinatorics :as combo]))

(defonce basic-colors {:red [255 0 0]
                       :green [0 255 0]
                       :yellow [255 255 0]
                       :blue [0 0 255]})

(defonce secondary-colors {:red [255 127 80]
                           :green [127 255 0]
                           :yellow [255 255 200]
                           :blue [0 191 255]})

(def edge 120)

(defonce start-points {:red [10 10]
                       :green [300 300]
                       :yellow [300 10]
                       :blue [10 300]})

(defn- populate-color-points [start-points edge color]
  (let [x (-> start-points color first)
        y (-> start-points color second)
        x-range (range x (+ x edge 1))
        y-range  (range y (+ y edge 1))]
    (combo/cartesian-product x-range y-range)))

(defn- max-xy [start-points]
  (map #(+ % edge) start-points))

(defn- point->button [button-points x y]
  (loop [color (first (keys button-points))
                   colors (next (keys button-points))]
              (when color
                (if (some #{(list x y)} (color button-points))
                  color
                  (recur (first colors)
                         (next colors))))))

(defn- dim-color [color]
  ;; Returns dimed coordinates
  )

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 1)

  ; Set color mode to HSB (HSV) instead of default RGB.
  #_(q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:colors basic-colors
   :button-points {:red (populate-color-points start-points edge :red)
                   :green (populate-color-points start-points edge :green)
                   :yellow (populate-color-points start-points edge :yellow)
                   :blue (populate-color-points start-points edge :blue)}})

(defn update-state [state]
  (log/info "GOT HERE")
  (log/info "Updated colors " (:colors state))

  state
  ; Update sketch state by changing circle color and position.
  #_{:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  (log/info "DRAWWWW")
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)

  ;; Set circle color.
  (apply q/fill (log/spy (:red (:colors state))))
  (q/rect (first (:red start-points)) (second (:red start-points)) edge edge)

  (apply q/fill (:yellow (:colors state)))
  (q/rect (first (:yellow start-points)) (second (:yellow start-points)) edge edge)

  (apply q/fill (:blue (:colors state)))
  (q/rect (first (:blue start-points)) (second (:blue start-points)) edge edge)

  (apply q/fill (:green (:colors state)))
  (q/rect (first (:green start-points)) (second (:green start-points)) edge edge))


(defn button-clicked [state event]
  (log/info "CLICKED AT" event)
  (let [color (point->button (:button-points state)
                             (:x event)
                             (:y event))]
    (log/info "Color: " color)
    (if color
      (assoc-in state [:colors color] (color secondary-colors))
      state)))

(q/defsketch simon
  :title "Simon says..."
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top :no-bind-output]
  :mouse-clicked button-clicked
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
