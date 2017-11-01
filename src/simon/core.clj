(ns simon.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.timbre :as log]))

(defonce colors {:red [255 0 0]
                 :green [0 255 0]
                 :yellow [255 255 0]
                 :blue [0 0 255]})

(defonce edge 120)

(defn setup []
  ; Set frame rate to 30 frames per second.
  #_(q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  #_(q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn update-state [state]
  state
  ; Update sketch state by changing circle color and position.
  #_{:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)

  ;; Set circle color.
  (apply q/fill (:red colors))
  (q/rect 10 10 edge edge)

  (apply q/fill (:yellow colors))
  (q/rect 300 10 edge edge)

  (apply q/fill (:blue colors))
  (q/rect 10 300 edge edge)

  (apply q/fill (:green colors))
  (q/rect 300 300 edge edge))


(defn button-clicked [state event]
  (log/info "GOT HERE" event)
  (clojure.pprint/pprint "lalalla")
  state)

(q/defsketch simon
  :title "Simon says..."
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  ;:update update-state
  :draw draw-state
  :features [:keep-on-top :no-bind-output]
  :mouse-clicked button-clicked
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
