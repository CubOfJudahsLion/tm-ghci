;; The name of the plugin depends on the OS.
(define (ghci-plug-in-binary)
  (if (getenv "SYSTEMROOT") ;; only defined in Windows
      "GHCIInterface"
      "GHCIInterface.bin"))


;;  Finds the documents directory for this plugin.
;;  Individual installs take precedence over global installs.
(define ghci-doc-dir
  (let* ((texmacs-paths      (list (getenv "TEXMACS_HOME_PATH") (getenv "TEXMACS_PATH")))
         (append-doc-subpath (lambda (s) (string-append s "/plugins/ghci/doc")))
         (possible-doc-paths (map append-doc-subpath texmacs-paths))
         (doc-paths          (filter file-exists? possible-doc-paths)))
    (if (= (length doc-paths) 0)
        #f
        (car doc-paths))))


;;
(plugin-configure ghci
  (:require (and
              (url-exists-in-path? "ghci")
              (url-exists-in-path? (ghci-plug-in-binary))))
  (:launch ,(ghci-plug-in-binary))
  (:session "GHCi"))


;;  Add the help file to the Help->Plugins menu.
(when (and supports-ghci? ghci-doc-dir)
  (tm-menu (help-plugins-menu)
           (former)
           ---
           ("GHCi" (load-help-article
                     (string-append ghci-doc-dir "/Help - The GHCi plug-in.tm")))))
