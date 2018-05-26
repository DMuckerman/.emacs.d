;; Better window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Use super for movement on OS X
(when (dan/mac?)
  (global-set-key (kbd "s-<left>") 'windmove-left)
  (global-set-key (kbd "s-<down>") 'windmove-down)
  (global-set-key (kbd "s-<up>") 'windmove-up)
  (global-set-key (kbd "s-<right>") 'windmove-right)
  ;; (global-set-key (kbd "s-h") 'windmove-left)
  ;; (global-set-key (kbd "s-j") 'windmove-down)
  ;; (global-set-key (kbd "s-k") 'windmove-up)
  ;; (global-set-key (kbd "s-l") 'windmove-right)

  ;; Better window resizing
  (global-set-key [(super shift k)] 'enlarge-window)
  (global-set-key [(super shift j)] 'shrink-window)
  (global-set-key [(super shift l)] 'enlarge-window-horizontally)
  (global-set-key [(super shift h)] 'shrink-window-horizontally)

  ;; Cmd-O to switch buffers
  (global-set-key (kbd "s-o") 'other-window))

;; Bring part of my Vim muscle memory to Emacs
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "C-S-o") 'vi-open-line-above)
