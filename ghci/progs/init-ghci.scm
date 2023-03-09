;; The name of the plugin depends on the OS.
(define (plug-in-binary)
  (if (getenv "SYSTEMROOT") ;; only defined in Windows
      "GHCIInterface"
      "GHCIInterface.bin"))


(plugin-configure ghci
  (:require (and
              (url-exists-in-path? "ghci")
              (url-exists-in-path? (plug-in-binary))))
  (:launch ,(plug-in-binary))
  (:session "GHCi"))
