;; Only Windows would set these variables. MinGW or Cygwin could
;; uppercase it.
(define is-windows? (or (getenv "SYSTEMROOT") (getenv "SystemRoot")))

;; The name of the plugin depends on the OS.
(define plug-in-bin
  (if (is-windows?)
      ("Interface.exe")
      ("Interface.bin")))


(plugin-configure haskell
  (:require (and
              (url-exists-in-path? "ghci")
              (url-exists-in-path? (plug-in-bin))))
  (:launch (plug-in-bin))
  (:session "GHCi")
  )
