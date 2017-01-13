
(defun split-window-func-with-other-buffer (split-function)
  "When splitting windows, show (other-buffer) in the new window"
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))

(defun split-window-horizontally-instead ()
  "Change split vertically to horizontally."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  "Change split horizontally to vertically."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun insert-line-before (times)
  "Insert a newline(s) above the line containing the cursor.
You might insert multiple lines using TIMES."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))

(defun insert-line-after (times)
  "Insert a newline(s) below the line containing the cursor.
You might insert multiple lines using TIMES."
  (interactive "p")
  (save-excursion
    (move-end-of-line 1)
    (newline times)))

(defun rzani/add-semicolon-end-of-line()
  "Add semicolon at the end of the line and return to current position"
  (interactive)
  (save-excursion
    (end-of-line)
    (if (not (= (preceding-char) 59)) (progn (insert ";") (evil-normal-state)))))

(defun rzani/add-comma-end-of-line()
  "Add comma at the end of the line and return to current position"
  (interactive)
  (save-excursion
    (end-of-line)
    (if (not (= (preceding-char) 44)) (progn (insert ",") (evil-normal-state)))))

(defun rzani/insert-single-arrow ()
  "Inserts -> at current cursor position"
  (interactive)
  (insert "->"))

(defun rzani/insert-double-arrow ()
  "Inserts => at current cursor position"
  (interactive)
  (insert "=>"))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)

(setq inhit-startup-message t)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(set-scroll-bar-mode nil)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(global-set-key (kbd "<f5>") 'revert-buffer)

(set-frame-font "Fira Mono for Powerline 13")   ; Change default font

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(use-package ag
  :ensure t
  :config (setq ag-executable "/usr/local/bin/ag"))

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("M-b" . helm-mini))
  :init (helm-mode 1)
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-autoresize-mode t
        helm-buffer-max-length 40)
  (defalias 'list-buffers 'helm-mini))

(use-package helm-projectile
  :ensure t
  :bind (("C-p" . helm-projectile-find-file))
  )

(use-package helm-ag
  :ensure t)

(use-package evil
  :ensure t
  :init
  (progn

    ;; evil-leader
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")
        (setq evil-leader/in-all-states t)

        (evil-leader/set-key
          "."   'mode-line-other-buffer
          "1"   'delete-other-windows
          "_"   'split-window-below
          "|"   'split-window-right
          "e"   'helm-find-files
          "f"   'helm-projectile-ag
          "p"   'insert-register
          "q"   'kill-this-buffer
          "r"   'helm-recentf
          "y"   'copy-to-register
          )))

    (evil-mode 1))
  :config
  (progn
    
    ;; use ido to open files
    ;;(define-key evil-ex-map "e " 'ido-find-file)
    ;;(define-key evil-ex-map "b " 'ido-switch-buffer)
    
    ;; Make escape quit everything, whenever possible.

    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
    (define-key evil-visual-state-map (kbd "C-p") 'helm-projectile-find-file)

    ;; change mode-line color by evil state
    (lexical-let ((default-color (cons (face-background 'mode-line)
                                       (face-foreground 'mode-line))))
      (add-hook 'post-command-hook
                (lambda ()
                  (let ((color (cond ((minibufferp) default-color)
                                     ((evil-insert-state-p) '("#ffc107" . "#ffffff"))
                                     ((evil-visual-state-p) '("#9c27b0" . "#ffffff"))
                                     ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                     ((buffer-modified-p)   '("#2196f3" . "#ffffff"))
                                     (t default-color))))
                    (set-face-background 'mode-line (car color))
                    (set-face-foreground 'mode-line (cdr color))))))
    ))

(use-package linum-relative
  :ensure t
  :bind (("<f7>" . linum-mode))
  :init (progn (global-linum-mode t) (linum-relative-mode t) )
  :config
  (progn
    (linum-mode)
    (custom-set-faces
     '(linum-relative-current-face ((t (:foreground "#a89984" :weight bold)))))
  ))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(use-package avy
  :ensure t
  :bind("M-s" . avy-goto-char-2 ))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

 (use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))

(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (color-theme-sanityinc-solarized-light))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (color-theme-sanityinc-solarized-dark))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :config (add-hook 'after-init-hook 'reapply-themes))
