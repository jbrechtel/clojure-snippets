(import '(javax.swing JFrame JLabel JTextField JButton JComboBox JPanel Timer)
        '(java.awt.event ActionListener)
        '(java.awt Color GridLayout))

(defn draw-sort [canvas alg]
  (prn alg))

(def maxval 100)

(def model (take 100 (repeatedly (fn [] (rand-int maxval)))))

(def canvas (proxy [JPanel] []
  (paintComponent [g]
    (proxy-super paintComponent g)
    (.setColor g Color/RED)
    (let [width (.getWidth this) height (.getHeight this) barHeight (/ height (inc (count model))) barWidthPerVal (/ width maxval)]
      (prn width height)
      (doseq [val (into (sorted-map) (zipmap (range 0 (count model)) model))] 
	(let [y (int (* (first val) barHeight)) barWidth (int (* (second val) barWidthPerVal))]
	  (.fillRect g 0 y barWidth barHeight)))))))

(defn sortapp []
  (let [frame (JFrame. "Sort Visualizer")
	algorithm-chooser (JComboBox.)
	run-button (JButton. "Run Algorithm")]
    (.addActionListener run-button
      (proxy [ActionListener] []
	(actionPerformed [evt] 
	 (draw-sort canvas (.getSelectedItem algorithm-chooser)))))
    (doto algorithm-chooser
      (.addItem "Quick sort")
      (.addItem "Bubble sort"))
    (doto frame
      (.setLayout (GridLayout. 2 2 3 3))
      (.add algorithm-chooser)
      (.add canvas)
      (.add run-button)
      (.setSize 300 300)
      (.setVisible true))))
	