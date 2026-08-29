
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-ghci.scm
;; DESCRIPTION : Initialize GHCi plugin
;; COPYRIGHT   : (C) 2023  Alexander Feterman Naranjo
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Binary name depends on OS.
(define (ghci-plugin-binary)
  (if (os-mingw?)
      "GHCIInterface"
      "GHCIInterface.bin"))

;; Plugin configuration
(plugin-configure ghci
  (:require (and
              (url-exists-in-path? "ghci")
              (url-exists-in-path? (ghci-plugin-binary))))
  (:launch ,(ghci-plugin-binary))
  (:session "GHCi"))

