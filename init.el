;; Set up Melpa
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(use-package delight
  :ensure t)

(defun dan/mac? ()
  "Returns `t` if this is an Apple machine, nil otherwise."
  (eq system-type 'darwin))

;; Load custom themes
(add-to-list 'custom-theme-load-path
	     (expand-file-name "~/.emacs.d/themes/"))
;; When using macOS
(when (dan/mac?)
  (setq mac-use-title-bar t)
  ;; Fix modifiers for Emacs Mac Port
  (setq mac-option-modifier 'meta
	mac-command-modifier 'super)
  ;; Set Cmd+V to paste for Emacs Mac Port
  ;; So macOS clipboard managers can still do their thing
  (global-set-key (kbd "s-v") 'yank)
  ;; Set default window size
  (add-to-list 'default-frame-alist '(height . 48))
  (add-to-list 'default-frame-alist '(width . 120)))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Theme settings
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init 
  (load-theme 'spacemacs-dark t))
  
;;(load-theme 'tron-dark t)
;; Disable pointless crap
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Fix some defaults
(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file "~/.emacs.d/custom.el")
;; Change font & set menu bar properly on macOS so fullscreen works
(if (dan/mac?)
    (progn
      (menu-bar-mode 1)
      (set-default-font "DejaVuSansMonoForPowerline Nerd Font-12"))
  (progn
    (menu-bar-mode -1)
    (setq default-frame-alist '((font . "DejaVuSansMonoForPowerline Nerd Font-10")))))

;;; Set up evil-mode
;; Enable undo-tree 
(use-package undo-tree
  :ensure t
  :delight
  :config
  (global-undo-tree-mode))
;; Enable evil-mode
;;(use-package evil
;;:ensure t
;;:config
;;(evil-mode 1))

;; Set up ;; god-mode
;; (use-package god-mode
;;   :ensure t
;;   :config
;;   (setq god-exempt-major-modes nil)
;;   (setq god-exempt-predicates nil)
;;   (global-set-key (kbd "<escape>") 'god-local-mode)
;;   (god-mode)

;;   (defun my-update-cursor ()
;;     (setq cursor-type (if (or god-local-mode buffer-read-only)
;; 			  'box
;; 			'bar)))

;;   (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;;   (add-hook 'god-mode-disabled-hook 'my-update-cursor)

;;   (define-key god-local-mode-map (kbd ".") 'repeat)
;;   (define-key god-local-mode-map (kbd "i") 'god-local-mode)

;;   (define-key god-local-mode-map [remap self-insert-command] 'my-god-mode-self-insert)

;;   (defun my-god-mode-self-insert ()
;;     (interactive)
;;     (if (and (bolp)
;; 	     (eq major-mode 'org-mode))
;; 	(call-interactively 'org-self-insert-command)
;;       (call-interactively 'god-mode-self-insert))))

;; Powerline settings
(load "~/.emacs.d/powerline.el")

;; Load some custom keys
(load "~/.emacs.d/keys.el")

(use-package aggressive-indent
  :delight
  :ensure t
  :config
  (aggressive-indent-global-mode))

;; Load helm
(load "~/.emacs.d/helm.el")

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)
	 ("C-c C-j" . avy-resume)))

(use-package smooth-scrolling
  :delight
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(use-package nlinum-relative
  :delight
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-redisplay-delay 0
	nlinum-relative-current-symbol ""
	nlinum-relative-offset 0))

(use-package company
  :delight
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Adapt company colors automatically to your theme
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
			    company-preview-frontend
			    company-echo-metadata-frontend)
	company-require-match 'never
	company-idle-delay 0.2
	company-tooltip-limit 20
	company-minimum-prefix-length 1
	company-echo-delay 0
	company-auto-complete nil
	company-selection-wrap-around t
	company-show-numbers t
	company-dabbrev-other-buffers t
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case nil
	company-tooltip-align-annotations t
	completion-styles '(basic initials partial-completion emacs22)) ; default is (basic partial-completion emacs22)

  ;; org-mode completions
  (defun my-pcomplete-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'my-pcomplete-capf)

  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1)))

(use-package alert
  :ensure t
  :config
  (if (dan/mac?)
      (setq alert-default-style 'osx-notifier)
    (setq alert-default-style 'libnotify))
  (use-package org-alert
    :ensure t))

(use-package zygospore
  :ensure t
  :bind (("C-x C-1" . zygospore-toggle-delete-other-windows)
	 ("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window)))

(use-package rotate
  :delight
  :ensure t)

(use-package clojure-mode
  :ensure t
  :defer
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))
  (add-hook 'clojure-mode-hook
            '(lambda ()
               ;; Hoplon functions and macros
               (dolist (pair '((page . 'defun)
                               (loop-tpl . 'defun)
                               (if-tpl . '1)
                               (for-tpl . '1)
                               (case-tpl . '1)
                               (cond-tpl . 'defun)))
                 (put-clojure-indent (car pair)
                                     (car (last pair)))))))

(use-package immortal-scratch
  :ensure t
  :delight
  :config
  (immortal-scratch-mode))

(use-package magit
  :ensure t
  :delight
  :bind (("C-x g" . magit-status)))

(use-package web-mode
  :ensure t
  :defer
  :config
  (add-hook 'web-mode-hook
	    (lambda ()
	      (define-key web-mode-edit-element-minor-mode-map (kbd "C-k") nil)
	      (define-key web-mode-edit-element-minor-mode-map (kbd "C-k") 'kill-line)))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (use-package web-mode-edit-element
    :ensure t
    :config
    (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))
  (use-package web-narrow-mode
    :ensure t
    :config
    (add-hook 'web-mode-hook 'web-narrow-mode)))

(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))


;; Load orgmode
(load "~/.emacs.d/org.el")

;; Load beancount file
(load "~/.emacs.d/beancount.el")
;; Load beancount related configs and functions
(load "~/.emacs.d/money.el")

(use-package emacs
  :delight
  (visual-line-mode))

;; (defun my-edit-file-as-root ()
;;   "Find file as root"
;;   (interactive)
;;   (let*
;;       ((sudo (/= (call-process "sudo" nil nil "-n true") 0))
;;        (file-name
;;         (if (tramp-tramp-file-p buffer-file-name)
;; 	    (with-parsed-tramp-file-name buffer-file-name parsed
;; 	      (tramp-make-tramp-file-name
;; 	       (if sudo "sudo" "su")
;; 	       "root"
;; 	       parsed-host
;; 	       parsed-localname
;; 	       (let ((tramp-postfix-host-format "|")
;;                      (tramp-prefix-format))
;; 		 (tramp-make-tramp-file-name
;;                   parsed-method
;;                   parsed-user
;;                   parsed-host
;;                   ""
;;                   parsed-hop))))
;;           (concat (if sudo
;; 		      "/sudo::"
;;                     "/su::")
;; 		  buffer-file-name))))
;;     (find-alternate-file file-name)))
(use-package php-mode
  :ensure t)

(setq tramp-default-method "ssh")
(server-start)
