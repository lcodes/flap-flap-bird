(ns flap-flap-bird.util
  (:import (java.io ByteArrayInputStream)
           (javax.sound.sampled AudioSystem Clip))
  (:require [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [quil.core :as q]))

;; Debugging

(def ^:dynamic *log* (agent nil))

(def bound-prn (bound-fn* prn))

(defn p* [_ args] (apply bound-prn args))

(defn p [& args] (send *log* p* args))

;; Math

(defn clamp [v min-v max-v]
  (cond (< v min-v) min-v
        (> v max-v) max-v
        :else       v))

;; Images

(defn load-image* [k] (-> k name (str ".png") io/resource q/load-image))

(defn load-image [m k] (assoc m k (load-image* k)))

(defn load-images [ks] (reduce load-image {} ks))

;; Assets

(def ^:dynamic *assets*)

(defn asset [k] (k *assets*))

;; Fonts

(defn font-key [size n] (keyword (str "font_" (name size) "_" n)))

(defn fonts [size] (for [n (range 10)] (font-key size n)))

(defn font [size n] (asset (font-key size n)))

;; Time

(defn now [] (-> (q/millis) double (/ 1000)))

(def ^:dynamic *time*)

;; Animation

(defn displacement
  ([time speed] (* time speed))
  ([time speed width] (mod (displacement time speed) width)))

(defn scroll-range [width] (-> (q/width) (/ width) q/ceil (+ 1) range))

(defn scroll [img y speed width]
  (let [d (displacement *time* speed width)]
    (doseq [x (scroll-range width)]
      (q/image img (-> x (* width) (- d)) y))))

;; Text

(defn char->int [c] (- (int c) (int \0)))

(defn text-positions [s x w] (range (- x (* w (count s))) (q/width) w))
