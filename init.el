;; init.el --- Here is the fun things begin!
;;
;;; Commentary:
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization
;; from Emacs Lisp embedded in literate Org-mode files.
;;

;;; Code:

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

(org-babel-load-file (expand-file-name "~/.emacs.d/init.el.org"))

;;; init.el ends here
