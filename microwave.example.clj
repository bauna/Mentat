(import '(ar.com.maba.tesis.microwave MicrowaveImpl))

; funciones auxiliares para ver mejore la traza
(defn simplify[[method enabledness :as pair]]
  [(g/method-label method)
   (reduce #(assoc %1 (g/method-label (first %2)) (second %2)) {} enabledness)])

(defn show-trace[l]
  (pprint (map simplify l)))

; construimos una función de seleccion:
;
;   [#mentat/while ("start" (not on))
;   #mentat/while ("openDoor" on)]
;
; recordar que se elegirá un método random cuando
; no se cumpla ninguna de las condiciones
(def sel-fn (s/generate-selection-function
              MicrowaveImpl
              (io/resource "mentat/microwaveTrapSel.edn")))

; construimos una traza usando ese función de selección
(def trace1 (t/trace-gen MicrowaveImpl sel-fn))

; mostramos que 5 pasos de la traza
(show-trace (take 5 trace1))

;Generamos el EPA
(def epa (g/build-dot-file 100 [trace1]))

;Mostramo el EPA
(-> epa d/dot d/show!)

(def trace2 (t/trace-gen MicrowaveImpl t/random-sel))
; mostramos que 5 pasos de la traza
(show-trace (take 5 trace2))

;Generamos el EPA
(def epa (g/build-dot-file 100 [trace1 trace2]))

;Mostramo el EPA
(-> epa d/dot d/show!)
