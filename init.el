;; @author rzani <rodrigo.zhs@gmail.com>

(setq ns-pop-up-frames nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'check-version)

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

(require 'functions)
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
