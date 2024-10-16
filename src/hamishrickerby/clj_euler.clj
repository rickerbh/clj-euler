;; ---------------------------------------------------------
;; hamishrickerby.clj-euler
;;
;; TODO: Provide a meaningful description of the project
;; ---------------------------------------------------------

(ns hamishrickerby.clj-euler
  (:gen-class)
  (:require
   [com.brunobonacci.mulog :as mulog]))

;; ---------------------------------------------------------
;; Application

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn greet
  "Greeting message via Clojure CLI clojure.exec"
  ([] (greet {:team-name "secret engineering"}))
  ([{:keys [team-name]}]
   (str "hamishrickerby clj-euler service developed by the " team-name " team")))


(defn -main
  "Entry point into the application via clojure.main -M"
  [& args]
  (let [team (first args)]
    (mulog/set-global-context!
     {:app-name "hamishrickerby clj-euler" :version  "0.1.0-SNAPSHOT"})
    (mulog/log ::application-starup :arguments args)
    (if team
      (println (greet team))
      (println (greet)))))

;; ---------------------------------------------------------


;; ---------------------------------------------------------
;; Rick Comment
#_{:clj-kondo/ignore [:redefined-var]}
(comment

  (-main)
  (-main {:team-name "Clojure Engineering"})

  #_()) ; End of rich comment block
;; ---------------------------------------------------------
