;;; Implementation of the Game of Life
;;; jeff.foster@acm.org

(ns gol.core
  (:gen-class)
  (:import (javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer))
  (:import (java.awt.event ActionListener MouseListener MouseAdapter MouseEvent))
  (:import (java.awt GridLayout Color)))

;; Rules from Wikipedia
;; 1. Any live cell with fewer than two live neighbours dies, as if by needs caused by underpopulation.
;; 2. Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 3. Any live cell with two or three live neighbours lives, unchanged, to the next generation.
;; 4. Any tile with exactly three live neighbours cells will be populated with a living cell.
(defstruct point :x :y)

(defn world-at [world point]
  (get world point))

(defn toggle-pos [world point]
  (if (zero? (world-at world point))
    (assoc world point 1)
    (assoc world point 0)))

(defn neighbours [p]
  [])

(defn neighbour-count [world p]
  0)

(defn new-state [world p]
  0)

(defn life-step [w]
  (into (hash-map) (map (fn [x] [(first x) (new-state w (first x))]) w)))

(defn create-world [w h]
  (let [x (range 0 w) y (range 0 h)]
    (apply hash-map (mapcat (fn [a] (mapcat (fn [b] (list (struct point a b) 0))  y)) x))))

;; UI elements and mutable ness
(def grid-size 15)

(def world (atom (create-world grid-size grid-size)))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (doseq [x (range 0 grid-size)]
      (doseq [y (range 0 grid-size)]
	(let [alive (world-at @world (struct point x y)) sq-size (/ (min (.getHeight this) (.getWidth this)) grid-size)]
	  (cond
	   (zero? alive) (.setColor g Color/BLUE)
	   :else (.setColor g Color/RED))
	  (.fillRect g (* x sq-size) (* y sq-size) (dec sq-size) (dec sq-size))))))))

(defn lifeapp []
  (swap! world (fn [w] (create-world grid-size grid-size)))
  (let [frame (JFrame. "Game of Life")]
    (doto canvas
      (.addMouseListener (proxy [MouseAdapter] []
        (mouseClicked [e]
          (if (= (MouseEvent/BUTTON1) (.getButton e))
	    (let [sq-size (/ (min (.getHeight canvas) (.getWidth canvas)) grid-size) x (int (/ (.getX e) sq-size)) y (int (/ (.getY e) sq-size))]
	      (swap! world (fn [w] (toggle-pos w (struct point x y)))))
	    (swap! world (fn [w] (life-step w))))
	  (.repaint canvas)))))
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setResizable true)
      (.setVisible true))))

(defn -main [& args]
  (lifeapp))
