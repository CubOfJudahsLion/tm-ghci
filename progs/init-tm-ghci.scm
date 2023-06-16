;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE     : init-tm-ghci.scm
;; DESCRIPTION: Initialization of GHCi plugin
;;              Haskell source importing
;; COPYRIGHT  : (C) 2023 Alexander Feterman-Naranjo <feterman at hotmail dot com>
;;
;; Distributed under the MIT license.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;  ==== GHCi session plugin ====


;;  Base name of the binary
(define (binary-name) "tm-ghci")


;; ;;  Session serializer
;; (define (ghci-serializer lan t)
;;   (string-append (verbatim-serialize lan t) "\n"))


;;  GHCi session plugin registration.
(plugin-configure ghci
  (:require (and (url-exists-in-path? "ghci")
                 (url-exists-in-path? (ghci-plug-in-binary))))
  (:launch ,(binary-name))
  (:session "GHCi")
  ;; (:scripts "GHCi")
  ;; (:tab-completion #t)
  )



;; ;;  ==== Haskell source importing ====
;;
;; ;;  Define and provide converters for Haskell
;; (define-format haskell
;;   (:name "Haskell Source Code")
;;   (:suffix "hs" "hsc"))
;;
;;  Reading a Haskell file will produce a single string with
;;  newline and/or return characters. Here we transform that
;;  into a list of strings.
;;(define (string-to-lines s)
;;  (unless (string? s)
;;    (error "String argument expected"))
;;  (let ((nl-re (make-regexp "\r\n?|\n|\f")))  ; the Haskell report specifies these as line ends.
;;    ;;  Replaces angle braces with proper symbols
;;    (define (fix-angle-braces s)
;;      (apply string-append
;;             (map (lambda (ch)
;;                    (cond
;;                      ((char=? #\< ch) "<less>")
;;                      ((char=? #\> ch) "<gtr>")
;;                      (else            (string ch))))
;;                  (string->list s))))
;;    ;;  Recursively split lines at newline chars
;;    (define (next-line acc rest)
;;      (if (string=? rest "")
;;          acc
;;          (let ((exec-res (regexp-exec nl-re rest)))
;;            (if (not exec-res)
;;                (append acc (list rest))
;;                (let* ((indexes   (array-ref exec-res 1))
;;                       (raw-line  (substring rest 0 (car indexes)))
;;                       (curr-line (fix-angle-braces raw-line))
;;                       (acc*      (append acc (list curr-line)))
;;                       (rest*     (substring rest (cdr indexes))))
;;                  (next-line acc* rest*))))))
;;    (next-line '() s)))
;;
;;
;; ;;  Extracts the texts from any tree
;; (define (tree-to-string t)
;;   (cond
;;     ((list? t)   (if (eqv? (length t) 0)
;;                      ""
;;                      (apply string-append
;;                             (map string-from-tree t))))
;;     ((string? t) t)
;;     (else        "")))
;;
;;
;; ;;  Places any Haskell source code inside tags that can
;; ;;  render anywhere.
;; (define (haskell-code-body s)
;;   `(code
;;      (with "font-base-size" "9"
;;            "font-size"      "1"
;;            (document
;;              ,@(string-to-lines s)))))
;;
;;
;; ;;  Takes a snippet and converts it into Haskell text
;; (define (stree->haskell t)
;;   (haskell-code-body (tree-to-string t)))
;;
;;
;; ;;  Turns a single-string Haskell document into a
;; ;;  TeXmacs stree.
;; (define (haskell->stree s)
;;   `(document
;;      (TeXmacs "2.1")
;;      (style "generic")
;;      (body ,(haskell-code-body s))
;;      (initial
;;        (collection
;;          (associate "preamble" "false")))) )
;;
;;
;; ;;  Register the converters.
;; (converter texmacs-snippet haskell-snippet
;;   (:function string-from-tree))
;;
;; (converter haskell-snippet texmacs-snippet
;;   (:function string-from-tree))
;;
;; (converter haskell-document texmacs-stree
;;   (:function haskell->stree))
;;
