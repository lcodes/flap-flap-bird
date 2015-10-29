(ns flap-flap-bird.core
  (:require [quil.core :as q]
            [quil.middleware :as qm]
            [flap-flap-bird.util :refer :all]))

;; Constant Values
;; -----------------------------------------------------------------------------

(def bg-speed 30)
(def fg-speed 125)

(def jump-velocity -4)
(def gravity       9.8)

(def bird-w 34)
(def bird-h 24)
(def bird-x 100)
(def bird-r 14)

(def ceiling-w 64)
(def ceiling-h 16)

(def land-w 336)
(def land-h 112)

(def sky-w 276)
(def sky-h 110)

(def pipe-h 26)
(def pipe-w 50)
(def pipe-distance 200)

(def game-h   420)
(def score-h  280)
(def splash-h 170)

(def replay-x        100)
(def replay-y-offset 200)
(def replay-w        115)
(def replay-h        70)

(def font-size {:small 14
                :big   26})

(def images (concat [:bird
                     :ceiling
                     :land
                     :pipe
                     :pipe-down
                     :pipe-up
                     :replay
                     :scoreboard
                     :sky
                     :splash]
                    (fonts :big)
                    (fonts :small)
                    (for [k [:bronze :silver :gold :platinum]]
                      (keyword (str "medal_" (name k))))))

(defn empty-game []
  {:assets        {}
   :real-time     (atom 0)
   :time          0
   :delta-time    0
   :bird-velocity 0
   :bird-y        0
   :pipes         []
   :score         0
   :high-score    0
   :origin        0
   :play?         false
   :dead?         false
   :click?        false
   :rewind?       false
   :history       ()})

;; Constant Helpers
;; -----------------------------------------------------------------------------

(defn align-y [h]
  (- (/ (q/height) 2) (/ h 2)))

(defn ground-height []
  (- (q/height) land-h))

(defn bird-ground-height []
  (- (ground-height) bird-r))

(defn ceiling-y []
  (- (ground-height) game-h))

(defn replay-y []
  (+ (align-y score-h) replay-y-offset))

(defn medal [score]
  (condp <= score
    200 :medal_platinum
    150 :medal_gold
    100 :medal_silver
    50 :medal_bronze
    nil))

(defn pipe-displacement [t]
  (displacement t fg-speed))

;; Update
;; -----------------------------------------------------------------------------

(defn intersect-point? [x1 x2 y1 y2 x y]
  (and (> x x1)
       (< x x2)
       (> y y1)
       (< y y2)))

(defn intersect-replay? []
  (let [x replay-x
        y (replay-y)]
    (intersect-point? x (+ x replay-w)
                      y (+ y replay-h)
                      (q/mouse-x) (q/mouse-y))))

(defn handle-input [{:keys [click? play? dead? time] :as game}]
  (cond-> game
    (and (not play?) click?)
    (assoc :play? true
           :score 0
           :origin (let [d (+ (q/width) (pipe-displacement time))]
                     (- d (mod d pipe-distance))))

    (and dead? click? (intersect-replay?))
    (assoc :play?         false
           :dead?         false
           :bird-velocity 0
           :pipes         []
           :history       ())

    :else identity))

(defn advance-time [{:keys [dead? time real-time] :as game}]
  (let [p @real-time
        t (now)
        d (- t p)]
    (reset! real-time t)
    (if dead?
      game
      (assoc game :time (+ time d) :delta-time d))))

(defn spawn-pipes* [pipes time]
  (let [d     (pipe-displacement time)
        width (q/width)
        kill  (- d pipe-w)
        pipes (->> pipes
               (filter (fn [[x _]] (> x kill)))
               (into []))]
    (if (= (count pipes) (-> width (/ pipe-distance) q/ceil inc))
      pipes
      (let [d (+ d width)
            o (or (-> pipes last first)
                  (- d (mod d pipe-distance)))]
        (conj pipes [(+ o pipe-distance) (- (rand-int 100) 50)])))))

(defn spawn-pipes [{:keys [play? time] :as game}]
  (if-not play?
    game
    (update-in game [:pipes] spawn-pipes* time)))

(defn bird-physics
  [{:keys [click? delta-time bird-y bird-velocity play? dead?] :as game}]
  (if-not play?
    (assoc game :bird-y (/ (q/height) 2))
    (let [v (if (and (not dead?) click?)
              jump-velocity
              (+ bird-velocity (* gravity delta-time)))]
      (assoc game
             :bird-velocity v
             :bird-y (clamp (+ bird-y (* 0.5 v))
                            (+ (ceiling-y) ceiling-h bird-r)
                            (bird-ground-height))))))

(defn intersect? [l r t b bird-y]
  (let [dist-x (- bird-x (clamp bird-x l r))
        dist-y (- bird-y (clamp bird-y t b))]
    (< (+ (* dist-x dist-x)
          (* dist-y dist-y))
       (* bird-r bird-r))))

(defn check-pipe-collision [bird-y pipes time]
  (when-let [[x h] (first pipes)]
    (let [d  (-> (pipe-displacement time))
          y  (q/height)
          hh (/ y 2)
          x1 (- x d)
          x2 (+ x1 pipe-w)
          y1 (+ h hh -50 pipe-h)
          y2 (+ h hh +50)]
      (or (intersect? x1 x2 0 y1 bird-y)
          (intersect? x1 x2 y2 y bird-y)))))

(defn check-collision [{:keys [bird-y play? pipes time origin] :as game}]
  (if (and play? (or (= bird-y (bird-ground-height))
                     (check-pipe-collision bird-y pipes time)))
    (assoc game :dead? true)
    game))

(defn update-score [{:keys [score high-score time origin] :as game}]
  (let [s (-> (pipe-displacement time)
              (- origin)
              (- pipe-w)
              (- bird-x)
              (/ pipe-distance)
              q/ceil)]
    (if (<= s score)
      game
      (assoc game :score s :high-score (max s high-score)))))

(defn reset-input [{:keys [click?] :as game}]
  (if click?
    (assoc game :click? false)
    game))

;; Render
;; -----------------------------------------------------------------------------

(defn text [s size x y]
  (let [w (font-size size)]
    (doseq [[c x] (map vector s (text-positions s x w))]
      (q/image (font size (char->int c)) x y))))

(defn background [_]
  (q/background (q/color 0x4E 0xC0 0xCA)))

(defn sky [_]
  (scroll (asset :sky) (- (q/height) land-h sky-h -1) bg-speed sky-w))

(defn land [_]
  (scroll (asset :land) (- (q/height) land-h) fg-speed land-w))

(defn ceiling [_]
  (scroll (asset :ceiling) (ceiling-y) fg-speed ceiling-w))

(defn pipes [{:keys [assets pipes time]}]
  (let [{:keys [pipe pipe-down pipe-up]} assets
        half-h (/ (q/height) 2)
        ceil   (+ (ceiling-y) ceiling-h)
        ground (ground-height)
        d      (pipe-displacement time)]
    (doseq [[x h] pipes]
      (let [x (- x d)
            y1 (+ half-h h -50)
            y2 (+ half-h h +50)]
        (q/image pipe-down x y1)
        (q/image pipe-up   x y2)
        (doseq [y (range ceil y1)]
          (q/image pipe x y))
        (doseq [y (range (+ y2 pipe-h) ground)]
          (q/image pipe x y))))))

(defn bird [{:keys [bird-y bird-velocity time]}]
  (let [y (* (-> time (* 12) q/ceil (mod 4)) bird-h)
        a (min q/HALF-PI (* bird-velocity 0.3))]
    (q/push-matrix)
    (q/translate bird-x bird-y)
    (q/rotate a)
    (q/translate (- (/ bird-w 2))
                 (- (/ bird-h 2)))
    (q/copy (asset :bird)
            [0 y bird-w bird-h]
            [0 0 bird-w bird-h])
    (q/pop-matrix)))

(defn splash [{:keys [play?]}]
  (when-not play?
    (q/image (asset :splash) 50 (align-y splash-h))))

(defn score [{:keys [play? dead? score]}]
  (when (and play? (not dead?))
    (text (str score) :big (- (q/width) 10) 10)))

(defn scoreboard [{:keys [dead? score high-score]}]
  (when dead?
    (let [y (align-y score-h)]
      (q/image (asset :scoreboard) 40 y)
      (text (str score)      :small 250 (+ y 105))
      (text (str high-score) :small 250 (+ y 145))
      (when-let [m (medal score)]
        (q/image (asset m) 73 (+ y 114))))))

(defn replay [{:keys [dead?]}]
  (when dead?
    (q/image (asset :replay) replay-x (replay-y))))

;; Game
;; -----------------------------------------------------------------------------

(defn game-setup []
  (assoc (empty-game) :assets (load-images images)))

(defn game-update [{:keys [play? dead? history rewind?] :as game}]
  (if rewind?
    (-> (first history)
        (or game)
        (assoc :rewind? true)
        advance-time)
    (let [game (-> game (or (game-setup))
                   handle-input
                   advance-time
                   spawn-pipes
                   bird-physics
                   check-collision
                   update-score
                   reset-input)]
      (if (or (not play?) dead?)
        game
        (update-in game [:history] #(take 600 (conj % game)))))))

(defn game-draw [{:keys [assets time] :as game}]
  (binding [*assets* assets
            *time*   time]
    (doto game
      background
      sky
      land
      ceiling
      pipes
      bird
      splash
      score
      scoreboard
      replay)))

(defn game-click [game {:keys [button]}]
  (if (= button :left)
    (assoc game :click? true)
    game))

(defn game-press [{:keys [play?] :as game} {:keys [raw-key]}]
  (if (and play? (= \space raw-key))
    (assoc game :rewind? true)
    game))

(defn game-release [game]
  (assoc game :rewind? false))

(q/defsketch flap-flap-bird
  :title "Flap Flap Bird"
  :size  [800 600]

  :setup         game-setup
  :update        game-update
  :draw          game-draw
  :mouse-clicked game-click
  :key-pressed   game-press
  :key-released  game-release

  :features   [:resizable]
  :middleware [qm/pause-on-error qm/fun-mode])
