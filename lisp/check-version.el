(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; create constant to check OS
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

(provide 'check-version)
