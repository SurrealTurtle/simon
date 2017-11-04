(ns simon.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [taoensso.timbre :as log]
            [clojure.math.combinatorics :as combo]))

(def basic-colors {:red [255 0 0]
                   :green [0 255 0]
                   :yellow [255 255 0]
                   :blue [0 0 255]})

(def secondary-colors {:red [255 127 80]
                       :green [152 251 152]
                       :yellow [255 255 150]
                       :blue [0 191 255]})

(defonce edge 120)

(defonce start-points {:red [10 10]
                       :green [300 300]
                       :yellow [300 10]
                       :blue [10 300]})
(def period 2)
(defonce high-score (atom 0))

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

(defn- animate [state color]
  (-> state
      (assoc-in [:colors color] (color secondary-colors))
      (assoc :counter period)))

(defn- reset [state]
  "Colors back to basic so we get blink effect"
  (if (zero? (:counter state))
    (assoc state
           :colors basic-colors
           :counter period)
    (update state :counter dec)))

(defn- random-color [color-list]
  (nth color-list (rand-int (count color-list))))

(defn- random-color-seq [color-list n]
  (for [x (range n)]
    (random-color color-list)))

(defn- produce-colors [state]
  (if (= (:n state) (:score state))
    (let [n (inc (:n state))]
      (-> state
          (assoc :n n)
          (assoc :current-series (random-color-seq (keys (:colors state)) n))))
    state))

(defn- display-color [state]
  (let [next-color (:next-color state)
        current-series (:current-series state)]
    (if (and (> (:n state) (:score state))
             (< next-color (count current-series)))
      (-> state
          (update :next-color inc)
          (animate-button (nth current-series (log/spy next-color))))
      state)))

(defn- reset-round [state]
  (-> state
      (assoc :current-series [])
      (update :n dec)
      (assoc :next-color 0)
      (assoc :next-input 0)))

(defn- maybe-update-high-score [score]
  (when (< @high-score score)
    (reset! high-score score)))

(defn- won-round? [state]
  (if (and (not-empty (:current-series state))
           (= (count (:current-series state))
              (:next-input state)))
    (let [new-score (inc (:score state))]
      (maybe-update-high-score new-score)
      (-> state
          (assoc :score new-score)
          (assoc :current-series [])
          (assoc :next-color 0)
          (assoc :next-input 0)))
    state))

(defn- correct-input? [state color]
  (if (= color (nth (:current-series state) (log/spy (:next-input state))))
    (update state :next-input inc)
    (reset-round state)))

(defn setup []
  ; Set frame rate to 4 frames per second.
  (q/frame-rate 4)
  
  {:colors basic-colors
   :button-points {:red (populate-color-points start-points edge :red)
                   :green (populate-color-points start-points edge :green)
                   :yellow (populate-color-points start-points edge :yellow)
                   :blue (populate-color-points start-points edge :blue)}
   :currenct-series []
   :next-color 0
   :next-input 0
   :score 0
   :n 0
   :counter period})

(defn update-state [state]
  #_(log/info "Updated colors " (:colors state))

  (log/info "coor ser " (count (:current-series state)))
  
  (-> state
      produce-colors
      display-color
      reset))

(defn draw-state [state]
  #_(log/info "DRAWWWW")

  ;; Clear the sketch by filling it with light-grey color.
  (q/background 240)

  (apply q/fill (:red (:colors state)))
  (q/rect (first (:red start-points)) (second (:red start-points)) edge edge)

  (apply q/fill (:yellow (:colors state)))
  (q/rect (first (:yellow start-points)) (second (:yellow start-points)) edge edge)

  (apply q/fill (:blue (:colors state)))
  (q/rect (first (:blue start-points)) (second (:blue start-points)) edge edge)

  (apply q/fill (:green (:colors state)))
  (q/rect (first (:green start-points)) (second (:green start-points)) edge edge)

  (q/fill 0 0 0)
  (q/text (str "Score: " (:score state)) 200 220)
  (q/text (str "High Score: " @high-score) 200 250))


(defn button-clicked [state event]
  #_(log/info "CLICKED AT" event)
  (let [color (point->button (:button-points state)
                             (:x event)
                             (:y event))]
    #_(log/info "Color: " color)
    ;; TODO: compare to series continue or abort
    (if color
      (-> state
          (correct-input? color)
          (won-round?)
          (animate-button color))
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
