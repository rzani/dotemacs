;; init.el --- Here is the fun things begin!
;;
;;; Commentary:
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization
;; from Emacs Lisp embedded in literate Org-mode files.
;;

;;; Code:

(require 'ob)          ;; org-mode export system
(require 'ob-tangle)   ;; org-mode tangling process

;; Check Emacs version
(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Install use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Set location of custom.el
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

;; Load custom file
(load custom-file 'noerror)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (perl . t)))

;; Load init.el.org and evaluate
(org-babel-load-file (expand-file-name "~/.emacs.d/init.el.org"))

;; Custom file's folder
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load functions.el
(require 'functions)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
