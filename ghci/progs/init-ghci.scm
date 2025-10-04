;; The name of the plugin depends on the OS.
(define (ghci-plug-in-binary)
  (if (getenv "SYSTEMROOT") ;; only defined in Windows
      "GHCIInterface"
      "GHCIInterface.bin"))

;; Plugin configuration
(plugin-configure ghci
  (:require (and
              (url-exists-in-path? "ghci")
              (url-exists-in-path? (ghci-plug-in-binary))))
  (:launch ,(ghci-plug-in-binary))
  (:session "GHCi"))

