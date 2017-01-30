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

(defun toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP")
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))


(provide 'functions)
