(defproject black-scholes "0.1.0-SNAPSHOT"
  :description "Black-Scholes GUI Calculator and Plotting"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.9.3"]
                 [seesaw "1.5.0"]]
  :main black_scholes.gui
  :repl-options {:init-ns black-scholes.core})
