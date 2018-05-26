;; Helm settings
(use-package helm
  :delight
  :ensure t
  :init
  (require 'helm-config)
  (helm-mode 1)

  (use-package helm-swoop
    :ensure t)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

  ;; Fix helm-man-woman issue on OS X
  (when (dan/mac?)
    (setq helm-man-format-switches "%s"))

  ;; Search manpages at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; From: https://github.com/hatschipuh/better-helm
  ;; Make helm always pop up at bottom
  (setq helm-split-window-in-side-p t)

  (add-to-list 'display-buffer-alist
	       '("\\`\\*helm.*\\*\\'"
		 (display-buffer-in-side-window)
		 (inhibit-same-window . t)
		 (window-height . 0.4)))

  (setq helm-swoop-split-with-multiple-windows nil
	helm-swoop-split-direction 'split-window-vertically
	helm-swoop-split-window-function 'helm-default-display-buffer)

  ;; Provide input in the header line and hide the mode-lines above
  (setq helm-echo-input-in-header-line t)

  (defvar bottom-buffers nil
    "List of bottom buffers before helm session.
Its element is a pair of `buffer-name' and `mode-line-format'.")

  (defun bottom-buffers-init ()
    (setq-local mode-line-format (default-value 'mode-line-format))
    (setq bottom-buffers
	  (cl-loop for w in (window-list)
		   when (window-at-side-p w 'bottom)
		   collect (with-current-buffer (window-buffer w)
			     (cons (buffer-name) mode-line-format)))))


  (defun bottom-buffers-hide-mode-line ()
    (setq-default cursor-in-non-selected-windows nil)
    (mapc (lambda (elt)
	    (with-current-buffer (car elt)
	      (setq-local mode-line-format nil)))
	  bottom-buffers))


  (defun bottom-buffers-show-mode-line ()
    (setq-default cursor-in-non-selected-windows t)
    (when bottom-buffers
      (mapc (lambda (elt)
	      (with-current-buffer (car elt)
		(setq-local mode-line-format (cdr elt))))
	    bottom-buffers)
      (setq bottom-buffers nil)))

  (defun helm-keyboard-quit-advice (orig-func &rest args)
    (bottom-buffers-show-mode-line)
    (apply orig-func args))


  (add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
  (add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
  (add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
  (add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
  (advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)

  ;; Hide header lines if only one source
  (setq helm-display-header-line nil)

  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

  (defun helm-toggle-header-line ()
    (if (> (length helm-sources) 1)
	(set-face-attribute 'helm-source-header
			    nil
			    :foreground nil
			    :background "#0d131f"
			    :box helm-source-header-default-box
			    :height 1.0)
      (set-face-attribute 'helm-source-header
			  nil
			  :foreground "#0d1519"
			  :background "#0d1519"
			  :box nil
			  :height 0.1)))


  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  ;; Hide minibuffer while Helm is active
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	(overlay-put ov 'window (selected-window))
	(overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
				`(:background ,bg-color :foreground ,bg-color)))
	(setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  ;; Reconfigure the behaviour of keys in helm file navigation buffers
  ;; Backspace goes to the upper folder if you are not inside a filename, and Return will select a file or navigate into the directory if it is one
  (defun dwim-helm-find-files-up-one-level-maybe ()
    (interactive)
    (if (looking-back "/" 1)
	(call-interactively 'helm-find-files-up-one-level)
      (delete-backward-char 1)))

  (define-key helm-read-file-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
  (define-key helm-read-file-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)
  (define-key helm-find-files-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
  (define-key helm-find-files-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)

  ;; Remove the dots in helm file navigation
  (require 'cl-lib)

  (with-eval-after-load 'helm-files
    (advice-add 'helm-ff-filter-candidate-one-by-one
		:around 'no-dots/helm-ff-filter-candidate-one-by-one)
    (advice-add 'helm-find-files-up-one-level
		:around 'no-dots/helm-find-files-up-one-level))

  (defvar no-dots-whitelist
    '()
    "List of helm buffers in which to show dots.")

  (defun no-dots/whitelistedp ()
    (member (with-helm-buffer (buffer-name)) no-dots-whitelist))

  (defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file)
    (when (or (no-dots/whitelistedp)
	      (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)))
      (funcall fcn file)))

  (defun no-dots/helm-file-completion-source-p (&rest args) t)

  (defun no-dots/helm-find-files-up-one-level (fcn &rest args)
    (prog2
	(advice-add 'helm-file-completion-source-p
		    :around 'no-dots/helm-file-completion-source-p)
	(apply fcn args)
      (advice-remove 'helm-file-completion-source-p
		     'no-dots/helm-file-completion-source-p))))
(use-package helm-flx
  :ensure t
  :init
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t
	helm-flx-for-helm-locate t))

(use-package projectile
  :delight
  :ensure t
  :config
  (projectile-global-mode)
  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    
    ;; Fix apparent bug with helm-projectile?
    :bind (:map projectile-command-map
		("s s" . helm-projectile-ag)
		("s g" . helm-projectile-grep)))
  (helm-projectile-on))
