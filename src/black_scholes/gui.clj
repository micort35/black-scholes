(ns black_scholes.gui
  (:use [seesaw.core])
  (:require
    [black_scholes.model :as bs]
    [incanter.core]
    [incanter.charts])
  (:gen-class)
  )

(defn display
  "Displays a single element"
  [f content]
  (config! f :content content)
  content)

(defn parse-text
  "Parse element text into float"
  [elem]
  (Float/parseFloat
    (text elem))
  )

(defn make-range
  "Create plotting range for variable"
  [x]
  (if (< 1 x)
    (range (/ x 5) (* x 5))
    (range (/ x 5) (* x 5) 0.05))
  )

(defn calc-which
  "Take all user input and calculate correct output for Black-Scholes equation"
  [option result spot strike volatility time rate]
  (if (compare (name option) "Call")
    (text! result (bs/call spot strike volatility time rate))
    (text! result (bs/put spot strike volatility time rate))
    )
  )

(defn create-plot
  [& {:keys [x option xvec spot strike volatility time rate]}]
  (if (compare option "Call")
    (incanter.charts/xy-plot xvec (bs/vector-call :spot spot :strike strike :volatility volatility
                                                  :time time :rate rate :vec x)
                             :x-label x
                             :y-label "Price")
    (incanter.charts/xy-plot xvec (bs/vector-put :spot spot :strike strike :volatility volatility
                                                 :time time :rate rate :vec x)
                             :x-label x
                             :y-label "Price")
    )
  )

(defn build-gui
  []
  ;style window to native OS
  (native!)

  ;create GUI frame
  (def f (frame :title "Black-Scholes Calculator"))

  ;option-type elements
  (def option-type (button-group))
  (def *call (radio :id "Call" :text "Call" :group option-type))
  (def *put (radio :id "Put" :text "Put" :group option-type))
  (def option-panel (flow-panel :items [*call *put]))
  ;calculation related elements
  (def *spot (text "Spot Price (USD)"))
  (def *strike (text "Strike Price (USD)"))
  (def *volatility (text "Implied Volatility"))
  (def *time (text "Time to Maturity"))
  (def *rate (text "Rate-free Interest"))
  (def calc (button :text "Calculate"))
  (def result (label :text "RESULT"))
  (def var-panel (vertical-panel :items [*spot *strike *volatility *time *rate calc result]))
  ;plotting elements
  (def x-axis (combobox :model ["Spot" "Strike" "Volatility" "Time" "Rate"]))
  (def plot (button :text "Plot"))
  (def plot-panel (flow-panel :items [x-axis plot]))
  ;wrap and display
  (def wrapper (vertical-panel :items [option-panel var-panel plot-panel]))
  (display f wrapper)

  ;register event handlers
  (listen calc :mouse-clicked
          (fn [e] (if (nil? (selection option-type))
                    (alert "Please select an option type!")
                    (calc-which (id-of (selection option-type))
                                result
                                (parse-text *spot)
                                (parse-text *strike)
                                (parse-text *volatility)
                                (parse-text *time)
                                (parse-text *rate)
                    )
            )
          )
          )


  (listen plot :mouse-clicked
          (fn [e]
            (cond
              (== (compare (selection x-axis) "Spot") 0) (incanter.core/view (create-plot
                                                                               :x (selection x-axis)
                                                                               :option (name (id-of (selection option-type)))
                                                                               :xvec (make-range (parse-text *spot))
                                                                               :spot (make-range (parse-text *spot))
                                                                               :strike (parse-text *strike)
                                                                               :volatility (parse-text *volatility)
                                                                               :time (parse-text *time)
                                                                               :rate (parse-text *rate)))
              (== (compare (selection x-axis) "Strike") 0) (incanter.core/view (create-plot
                                                                                 :x (selection x-axis)
                                                                                 :option (name (id-of (selection option-type)))
                                                                                 :xvec (make-range (parse-text *strike))
                                                                                 :spot (parse-text *spot)
                                                                                 :strike (make-range (parse-text *strike))
                                                                                 :volatility (parse-text *volatility)
                                                                                 :time (parse-text *time)
                                                                                 :rate (parse-text *rate)))
              (== (compare (selection x-axis) "Volatility") 0) (incanter.core/view (create-plot
                                                                                     :x (selection x-axis)
                                                                                     :option (name (id-of (selection option-type)))
                                                                                     :xvec (make-range (parse-text *volatility))
                                                                                     :spot (parse-text *spot)
                                                                                     :strike (parse-text *strike)
                                                                                     :volatility (make-range (parse-text *volatility))
                                                                                     :time (parse-text *time)
                                                                                     :rate (parse-text *rate)))
              (== (compare (selection x-axis) "Time") 0) (incanter.core/view (create-plot
                                                                               :x (selection x-axis)
                                                                               :option (name (id-of (selection option-type)))
                                                                               :xvec (make-range (parse-text *time))
                                                                               :spot (parse-text *spot)
                                                                               :strike (parse-text *strike)
                                                                               :volatility (parse-text *volatility)
                                                                               :time (make-range (parse-text *time))
                                                                               :rate (parse-text *rate)))
              (== (compare (selection x-axis) "Rate") 0) (incanter.core/view (create-plot
                                                                               :x (selection x-axis)
                                                                               :option (name (id-of (selection option-type)))
                                                                               :xvec (make-range (parse-text *rate))
                                                                               :spot (parse-text *spot)
                                                                               :strike (parse-text *strike)
                                                                               :volatility (parse-text *volatility)
                                                                               :time (parse-text *time)
                                                                               :rate (make-range (parse-text *rate))))
              )
            )
          )

  ;pack contents and display
  (-> f pack! show!)
          )


(defn -main
  []
  (build-gui)
  )