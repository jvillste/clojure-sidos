(ns org.sidos.browser
  (:import
   (java.awt Color Dimension Graphics2D RenderingHints Font)
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)
   (javax.swing JFrame JOptionPane JPanel)))

                                        ;(set! *warn-on-reflection* true)

;; painting

(defprotocol Painter
  (rectangle [painter x y width height])
  (text [painter text x y]))

;; graphical context

(defprotocol GraphicalContext
  (text-dimensions [graphical-context text]))

;; layout

(defn box-layout-parent [parent child margin]
  (assoc parent
     :width (+ (* 2 margin) (:width child))
     :height (+ (* 2 margin) (:height child))))

(defn box-layout-child [parent child margin]
  (assoc child
     :x (+ margin (:x parent))
     :y (+ margin (:y parent))))


;; --- components ---

(defprotocol Component
  (draw [component painter])
  (layout [component graphical-context])
  (handle-input [component input])
  (hit? [component graphical-context x y]))

;; rectangle

(defn rectangle-draw [rectangle painter]
  (rectangle painter
             (:x rectangle)
             (:y rectangle)
             (:width rectangle)
             (:height rectangle)))

(defn rectangle-layout [rectangle graphical-context]
  (assoc rectangle
    :width 0
    :height 0))

(defn rectangle-hit? [rectangle graphical-context x y]
  (and (> x (:x rectangle))
       (> y (:y rectangle))
       (< x (+ (:x rectangle)
               (:width rectangle)))
       (< y (+ (:y rectangle)
               (:height rectangle)))))

(defrecord Rectangle [])

(extend Rectangle Component
        {:draw rectangle-draw
         :layout rectangle-layout
         :hit? rectangle-hit?
         :handle-input (fn [])})

;; text

(defn text-draw [text painter]
  (text painter
        (:text text)
        (:x text)
        (:y text)))

(defn text-layout [text graphical-context]
  (let [dimensions (text-dimensions graphical-context (:text text))]
    (assoc text
      :width (:width dimensions)
      :height (:height dimensions))))

(defrecord Text [text])

(extend Text Component
        {:draw text-draw
         :layout text-layout
         :hit? rectangle-hit?
         :handle-input (fn [])})

;; text input

(defn layout-text-input [text-input]
  (let [[new-rectangle-component new-text-component] (box-layout (:rectangle-component text-input)
                                                                 (:text-component text-input))]
    (assoc text-input
      :rectangle-component new-rectangle-component
      :text-component new-text-component
      :width (:width new-rectangle-component)
      :height (:height new-rectangle-component))))

(defn draw-text-input [text-input painter]
  (draw (:rectangle-component text-input) painter)
  (draw (:text-component text-input) painter))

(defn create-text-input-children [text-input]
  (assoc text-input
    :text-component (Text. (:text text-input))
    :retangle-component (Rectangle.)))

(defn text-input-handle-input [text-input input-state]
  (if (=  (:type (:last-event input-state))
          :key-pressed)
    (-> text-input
        (assoc :text (str (:text text-input) (:key-character (:last-event input-state))))
        (create-text-input-children)
        (layout-text-input))

    text-input))

(defrecord TextInput [])

(defn new-text-input []
  (create-text-input-children (TextInput.)))

(extend TextInput Component
        {:draw draw-text-input
         :handle-input text-input-handle-input
         :layout layout-text-input
         :hit? rectangle-hit?})

;; input



(defn new-input-state []
  {:time (System/nanoTime)
   :keys-down #{}
   :left-mouse-button-down false
   :middle-mouse-button-down false
   :right-mouse-button-down false
   :mouse-x 0
   :mouse-y 0})

(def input-state (agent (new-input-state)))

(defn notify-input-event [new-input-state]
  (swap! *components* (fn [components]
                        (doall (map #(handle-input % new-input-state)
                                    components))))
  (.repaint panel))


(defn handle-input [state-changes event]
  (send-off input-state (fn [input-state]
                          (let [new-input-state (apply assoc input-state
                                                       (concat state-changes
                                                               [:last-event event
                                                                :time (System/nanoTime)]))]
                            (notify-input-event new-input-state)
                            new-input-state))))

(def mouse-input-handler
  (proxy [MouseAdapter] []
    (mousePressed  [e]  (case (.getButton e)
                              1 (handle-input [:left-mouse-button-down true]
                                              {:type :left-mouse-button-down})
                              2 (handle-input [:middle-mouse-button-down true]
                                              {:type :middle-mouse-button-down})
                              3 (handle-input [:right-mouse-button-down true]
                                              {:type :right-mouse-button-down})))

    (mouseReleased [e] (case (.getButton e)
                             1 (handle-input [:left-mouse-button-down false]
                                             {:type :left-mouse-button-up})
                             2 (handle-input [:middle-mouse-button-down false]
                                             {:type :middle-mouse-button-up})
                             3 (handle-input [:right-mouse-button-down false]
                                             {:type :right-mouse-button-up})))
    (mouseEntered [e] )
    (mouseExited [e] )
    (mouseMoved [e] (handle-input [:mouse-x (.getX e) :mouse-y (.getY e)]
                                  {:type :mouse-moved}))
    (mouseDragged [e] )))

(def keyboard-input-handler
  (proxy [KeyAdapter] []
    (keyPressed [e] (handle-input [:keys-down (conj (:keys-down @input-state) (.getKeyCode e))]
                                  {:type :key-pressed
                                   :key-code (.getKeyCode e)
                                   :key-character (.getKeyChar e)}))
    (keyReleased [e] (handle-input [:keys-down (disj (:keys-down @input-state) (.getKeyCode e))]
                                   {:type :key-released
                                    :key-code (.getKeyCode e)
                                    :key-character (.getKeyChar e)}))))



;; browser

(def *components* (atom []))

(def *width* 400)
(def *height* 400)

(def last-frame-time (System/nanoTime))

(defn render [g]
  (doto g
    (.setColor (. Color white))
    (.fillRect 0 0 *width* *height*)
    (.setColor (. Color blue)))
  (dorun (map #(draw % g) @*components*))

  (.dispose g))

(def panel (doto (proxy [JPanel] []
                   (paint [g] (render g)))
             (.setPreferredSize (new Dimension 200 200))
             (.addMouseListener mouse-input-handler)
             (.addMouseMotionListener mouse-input-handler)
             (.addKeyListener keyboard-input-handler)
             (.setFocusable true)
             (.requestFocusInWindow)))

;; Java2D


(extend-type Graphics2D

  Painter
  (rectangle [graphics2d x y width height]
    (.drawRect graphics2d x y width height))
  (text [graphics2d text x y]
    (.setFont graphics2d (Font. "Arial" Font/PLAIN 12))
    (.setRenderingHint graphics2d
                       RenderingHints/KEY_TEXT_ANTIALIASING
                                        ;RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HRGB
                       RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                       )
    (.drawString graphics2d text x y))

  GraphicalContext
  (text-dimensions [graphics2d text]
    (let [font-metrics (.getFontMetrics graphics2d (.getFont graphics2d))]
      (println (.stringWidth font-metrics text))
      {:width (.stringWidth font-metrics text)
       :height (.getHeight font-metrics)
       })))




;; test view

(comment

  (box box-1
       (text-input name))

  (text-input/on-change name [new-value]
                        (println new-value))


  (style text-input
         :font-face "Times"
         :font-size 10)

  (style box-1
         :padding 10
         :background-color (color 0 0 1))

  )

(swap! *components* #(conj % (assoc (new-text-input)
                               :x 0
                               :y 0)))

(def frame (doto (new JFrame)
             (.add panel)
             (.addWindowListener
              (proxy [WindowAdapter] []
                (windowClosing [e]
                  (println "Frame closed")
                  (.dispose frame))))
             .pack
             .show))

(comment

  (def *components* (repeatedly 100 create-ship))

  ;; Animation

  (def ui (agent nil))

  (def running true)

  (defn animation [x]
    (when running (send-off *agent* #'animation))
    (update)
    (.repaint panel)
    (Thread/sleep 10)
    nil)




  (send-off ui animation)

  (def running false)

  )

