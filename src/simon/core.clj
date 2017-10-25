(ns simon.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.timbre :as log]))

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
  (q/fill 255 0 0)
  (q/rect 10 10 100 100)

  (q/fill 0 255 0)
  (q/rect 200 10 100 100)

  (q/fill 0 0 255)
  (q/rect 10 200 100 100)

  (q/fill 255 255 0)
  (q/rect 200 200 100 100)
  ; Calculate x and y coordinates of the circle.
  #_(let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse x y 100 100))))


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
