; spot (double) - current price in USD, e.g. $31.93
; strike (double) - exercise price in USD, e.g. $32.50
; volatility (double) - percent volatilty of market, e.g. .32 (32%)
; time (double) - time left, in years, e.g. 40 days = 40/365 = ~.11
; rate (double) - interest rate, e.g. .04 (4%)

(ns black_scholes.model
  (:require incanter.distributions))

(defn d1
  [spot strike volatility time rate]
  (/
    (+ (Math/log (/ spot strike))
       (* time (+ rate (/ (* volatility volatility) 2)))
       )
    (* volatility (Math/sqrt time))
    )
  )

(defn d2
  [spot strike volatility time rate]
  (- (d1 spot strike volatility time rate)
     (* volatility (Math/sqrt time))
     )
  )

(defn ndist
  [input]
  (incanter.distributions/cdf (incanter.distributions/normal-distribution) input)
  )

(defn call
  [spot strike volatility time rate]
  (- (* spot (ndist (d1 spot strike volatility time rate)))
     (* strike (Math/exp (* (- rate) time)) (ndist (d2 spot strike volatility time rate)))
     )
  )

(defn put
  [spot strike volatility time rate]
  (- (* strike (Math/exp (* (- rate) time)) (ndist (- (d2 spot strike volatility time rate))))
     (* spot (ndist (d1 spot strike volatility time rate)))
     )
  )

(defn extend-to
  "Creates a vector of repeated values of the same length as input vector"
  [val vec]
  (take (count vec) (repeat val))
  )

(defn vector-call
  "Creates a sequence of calculations with call equation"
  [& {:keys [spot strike volatility time rate vec]}]
  (cond
    (== (compare vec "Spot") 0) (map call spot (extend-to strike spot) (extend-to volatility spot)
                              (extend-to time spot) (extend-to rate spot))
    (== (compare vec "Strike") 0) (map call strike (extend-to spot strike) (extend-to volatility strike)
                                (extend-to time strike) (extend-to rate strike))
    (== (compare vec "Volatility") 0) (map call volatility (extend-to spot volatility) (extend-to strike volatility)
                                    (extend-to time volatility) (extend-to rate volatility))
    (== (compare vec "Time") 0) (map call time (extend-to spot time) (extend-to strike time)
                              (extend-to volatility time) (extend-to rate time))
    (== (compare vec "Rate") 0) (map call rate (extend-to spot rate) (extend-to strike rate)
                              (extend-to volatility rate) (extend-to time rate))
    )
  )

(defn vector-put
  "Creates a sequence of calculations with call equation"
  [& {:keys [spot strike volatility time rate vec]}]
  (cond
    (== (compare vec "Spot") 0) (map put spot (extend-to strike spot) (extend-to volatility spot)
                              (extend-to time spot) (extend-to rate spot))
    (== (compare vec "Strike") 0) (map put strike (extend-to spot strike) (extend-to volatility strike)
                                (extend-to time strike) (extend-to rate strike))
    (== (compare vec "Volatility") 0) (map put volatility (extend-to spot volatility) (extend-to strike volatility)
                                    (extend-to time volatility) (extend-to rate volatility))
    (== (compare vec "Time") 0) (map put time (extend-to spot time) (extend-to strike time)
                              (extend-to volatility time) (extend-to rate time))
    (== (compare vec "Rate") 0) (map put rate (extend-to spot rate) (extend-to strike rate)
                              (extend-to volatility rate) (extend-to time rate))
    )
  )