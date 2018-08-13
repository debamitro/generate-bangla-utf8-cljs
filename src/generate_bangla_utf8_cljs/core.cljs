(ns generate-bangla-utf8-cljs.core
  )

(defn handle-convert-request
  []
  (js/console.log "clicked convert")
  )

(defn attach-events
  []
  (.addEventListener
   (js/document.querySelector "#convertButton")
   "click"
   handle-convert-request
   )
  )

(js/ready attach-events)


;; (js/console.log "Hah")

;; This is for nodejs
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (js/console.log "Hello, World!"))

(set! *main-cli-fn* -main)
