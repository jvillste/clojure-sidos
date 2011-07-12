(ns org.sidos.browser
  (:import
   (java.awt Color Dimension Graphics2D)
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)
   (javax.swing JFrame JOptionPane JPanel)))

                                        ;(set! *warn-on-reflection* true)


;; painting

(defprotocol Painter
  (rectangle [painter x y width height])
  (text [painter text x y]))

;; component

(defprotocol Component
  (handle-input [component input])
  (draw [component painter]))

(def *components* (atom []))

;; browser

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
             (.addMouseListener
              mouse-input-handler)
             (.addMouseMotionListener
              mouse-input-handler)
             (.addKeyListener
              keyboard-input-handler)
             (.setFocusable true)
             (.requestFocusInWindow)))

;; Java2D


(extend-type Graphics2D
  Painter
  (rectangle [graphics2d x y width height]
    (.fillRect graphics2d x y width height))
  (text [graphics2d text x y]
    (.drawString graphics2d text x y)))

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



;; layout

(defprotocol Layout
  (layout [layout element]))

(defrecord BoxLayout []
  Layout
  (layout [box-layout element]))

;; text box

(defn draw-text-box [text-box painter]
  (rectangle painter
             (:x text-box)
             (:y text-box)
             (:width text-box)
             (:height text-box))

  (text painter
        (:text text-box)
        (:x text-box)
        (:y text-box)))

(defrecord TextBox [x y width height text]
  Component
  (draw [text-box painter] (draw-text-box text-box painter))
  (handle-input [text-box input-state]
    (if (=  (:type (:last-event input-state))
            :key-pressed)
      (assoc text-box :text (str (:text text-box) (:key-character (:last-event input-state))))
      text-box)))

;; test view

(comment

  (box box-1
       (text-box name))

  (text-box/on-change name [new-value]
                      (println new-value))


  (style text-box
         :font-face "Times"
         :font-size 10)

  (style box-1
         :padding 10
         :background-color (color 0 0 1))

  )

(swap! *components* #(conj % (TextBox. 10 10 100 100 "Foo")))

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

