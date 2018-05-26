;; Set up Tron theme for powerline
(defface powerline-tron-active1 '((t (:foreground "black" :background "#a0ffff" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-tron-active2 '((t (:foreground "#eeeeec" :weight bold :background "#0d131f" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-tron-active3 '((t (:foreground "black" :background "#00d4d4" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-tron-inactive1 '((t (:foreground "white" :background "black" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defun powerline-tron-theme ()
  "Setup the default mode-line."
  (interactive)
  (if (not window-system)
      (progn
	(setq powerline-default-separator 'utf-8)
	(setq powerline-utf-8-separator-left #xe0b8)
	(setq powerline-utf-8-separator-right #xe0ba)))
  (if window-system
      (setq powerline-default-separator 'slant))
  ;;(setq powerline-arrow-shape 'curve)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-tron-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-tron-active2 'powerline-inactive2))
			  (face3 (if active 'powerline-tron-active3 'powerline-tron-inactive1))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))

			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" face3 'l)
				     (powerline-buffer-id face3 'l)
				     (when (and (boundp 'which-func-mode) which-func-mode)
				       (powerline-raw which-func-format face3 'l))
				     (powerline-raw " " face3)
				     (funcall separator-left face3 face1)
				     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
				       (powerline-raw erc-modified-channels-object face1 'l))
				     (powerline-major-mode face1 'l)
				     (powerline-process face1)
				     (powerline-minor-modes face1 'l)
				     (powerline-narrow face1 'l)
				     (powerline-raw " " face1)
				     (funcall separator-left face1 face2)
				     (powerline-vc face2 'r)
				     (when (bound-and-true-p nyan-mode)
				       (powerline-raw (list (nyan-create)) face2 'l))))
			  (rhs (list (powerline-raw global-mode-string face2 'r)
				     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 face3)
				     (powerline-raw " " face3)
				     (powerline-raw "%6p" face3 'r)
				     (when powerline-display-hud
				       (powerline-hud face3 face2)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(use-package powerline
  :ensure t
  :config
  ;; Modeline clock customization
  ;; Show clock in modeline
  (setq display-time-format (concat (char-to-string #x25F7) "%k:%M  "))
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  ;; Modeline battery customization
  ;; Show battery in modeline
  (setq battery-mode-line-format (concat (char-to-string #x26a1) "%p%%%% (%t)"))
  (display-battery-mode 1)
  (setq powerline-default-separator 'slant)
  ;; (use-package powerline-evil
  ;;   :ensure t
  ;;   :config
  ;;   (powerline-evil-vim-color-theme)))
  (powerline-default-theme))
