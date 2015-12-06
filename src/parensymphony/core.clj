(ns parensymphony.core
  (use [overtone.live])
  (require [quil.core :as q]
           [quil.middleware :as m]))

(def window-width 1000)
(def window-height 1000)


(definst beep [freq 440]
  (-> freq
      saw
      (* (env-gen (perc) :action FREE))))

(definst ping [freq 440]
  (-> freq
      square
      (* (env-gen (perc) :action FREE))))

(definst seeth [freq 440 dur 1.0]
  (-> freq
      saw
      (* (env-gen (perc (* dur 1/2) (* dur 1/2)) :action FREE))))

(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq   (midicps note)
        noize  (* 0.8 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize gate dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action FREE) reverb)))

(def prog-55 [(chord :A2 :minor) (chord :F2 :major) (chord :G2 :major) (chord :A2 :minor)])

(defn play-chord [inst a-chord]
  (doseq [note a-chord] (inst note :dur 8)))

(defn play-chord-with-key [{:keys [chord-index chord-root chord-name]}]
  (play-chord plucked-string (nth prog-55 chord-index)))

(defn play-with-key [{:keys [scale-index scale-root scale-name]}]
  (plucked-string (nth (scale scale-root scale-name) scale-index)))

(defn inc-index [index]
  (rem (inc index) 8))

(defn inc-chord-index [index]
  (rem (inc index) 4))

(defn setup []
  (q/smooth)
  (q/background 0)
  {:pressed-key ""
   :scale-index 0 :scale-root :E3 :scale-name :phrygian
   :chord-index 0 :chord-root :C3 :chord-name :major
   :code ""})

(defn draw [state]
  (q/background 0)
  (q/fill 255)

  (q/text (str (:pressed-key state)) 20 20)
  (q/text (:code state) 20 100))

(defn insert-char [code char]
  (str code char))

(defn delete-backward-char [code]
  (subs code 0 (dec (count code))))

(defn insert-paren [code]
  (str code "()"))

(defn key-pressed [state event]
  (let [c (str (q/key-as-keyword))]
    (println (q/raw-key))
    (cond
      (= c ":(" ) (do (play-chord-with-key state)
                      (-> state
                          (update-in [:chord-index] inc-chord-index)
                          (assoc :code (insert-paren (:code state)))))
      (= c ": ") (assoc state :code (insert-char (:code state) (q/raw-key)))
      (= c ":shift") state
      (= (q/key-code) 8) (do (play-with-key state)
                             (-> state
                                 (update-in [:scale-index] inc-index)
                                 (assoc  :code (delete-backward-char (:code state)))))
      :else (do (play-with-key state)
                (-> state
                    (update-in [:scale-index] inc-index)
                    (assoc  :code (insert-char (:code state) (q/raw-key))))))))


(q/defsketch parensymphony
  :title "symphony"
  :setup setup
  :draw draw
  :key-pressed key-pressed
  :renderer :p2d
  :middleware [m/fun-mode m/pause-on-error]
  :size [window-width window-height])
