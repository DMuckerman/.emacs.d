(require 'org)
(require 'org-capture)
(require 'org-protocol)

;; Enfore todo dependencies
(setq org-enforce-todo-dependencies t)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Nextcloud/Org/")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

;; Save all open Org buffers every hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (org-file-path "index.org"))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file (org-file-path "index.org"))
	       "* TODO %?\n%U\n%a\n")
	      ("n" "Note" entry (file (org-file-path "notes.org"))
	       "* %? \n%U\n%a\n")
	      ("r" "Respond" entry (file (org-file-path "index.org"))
	       "* TODO Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"))))

;; Set up Org keywords and shortcut keys
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
			  (sequence "WAITING(w@/!)" "|")
			  (sequence "|" "CANCELED(c@)")))

(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)

;; Colorize the newer key words
(setq org-todo-keyword-faces '(("WAITING"  . (:foreground "violet" :weight bold))
			       ("CANCELED" . (:foreground "brown" :weight bold))))

(use-package org-autolist
  :delight
  :ensure t)
(use-package org-bullets
  :delight
  :ensure t)
(use-package helm-org-rifle
  :delight
  :ensure t 
  :bind (("C-c r" . helm-org-rifle)))

;; Set up a few display options and keybindings
(use-package visual-fill-column
  :delight
  :ensure t)
(use-package orgtbl-show-header
  :delight
  :ensure t)
(add-hook 'org-mode-hook
	  (lambda ()
	    (set-fill-column 120)
	    (visual-line-mode 1)
	    (visual-fill-column-mode 1)
	    (org-bullets-mode 1)
	    (org-autolist-mode)
	    (orgtbl-show-header)))

;; Set agenda property column
(setq org-agenda-property-column 70)

;; Refresh agenda buffer periodically
(defun renewOrgBuffer ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
	(org-agenda-maybe-redo))))
  )
(run-with-idle-timer 3 1000 #'renewOrgBuffer)

;; Change org ellipsis
(setq org-ellipsis "⤵")

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t)
   (emacs-lisp . t)
   (perl . t)
   (sql . t)
   (ditaa . t)
   ))
;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; Set up agenda files
(setq org-agenda-files (list (org-file-path "index.org")
			     (org-file-path "projects.org")
			     (org-file-path "notes.org")))

;; Useful global hotkeys for Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Make org-agenda not trash my window setup
(setq org-agenda-window-setup 'other-window)

;; Location of main Org todo file
(setq org-index-file (org-file-path "index.org"))

;; Location of main Org todo file
(setq org-inbox-file (org-file-path "refile-beorg.org"))

;; Set archive location for finished tasks
(setq org-archive-location
      (concat (org-file-path "archive") "::* From %s"))

;; Pull tasks from Org inbox to Org index file
(defun dan/copy-tasks-from-inbox ()
  (interactive)
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

(global-set-key (kbd "C-c i") 'dan/copy-tasks-from-inbox)

;; Custom agendas
(setq org-agenda-custom-commands
      '(("c" "Calendar" agenda ""
         ((org-agenda-ndays 7)                          ;; [1]
          (org-agenda-start-on-weekday 0)               ;; [2]
          (org-agenda-time-grid nil)
          (org-agenda-repeating-timestamp-show-all t)   ;; [3]
          (org-agenda-entry-types '(:timestamp :sexp))));; [4]
	("d" "Today" agenda ""
	 ((org-agenda-ndays 1)
	  (org-deadline-warning-days 0)))
	("N" "Notes" tags "NOTE"
	 ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))
	))

;;; Set agendas to show up in MobileOrg
;; I've switched to beorg, but this is handy in case I need to fall back
;;  or if beorg ever supports custom agendas
;; (setq org-mobile-agendas '("d" "a" "t" "c"))

;; Improve refiling ability
(setq org-refile-targets
      '((nil :maxlevel . 3)
	(org-agenda-files :maxlevel . 3)))
;; Allow you to refile to other agenda files as top level headings
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;; Allow creating new nodes during refile
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Set up org capture
(defvar my-org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq my-org-capture-before-config (current-window-configuration)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (when (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise org-refile to close the frame"
  (delete-frame))

(defadvice org-capture-select-template (around delete-capture-frame activate)
  "Advise org-capture-select-template to close the frame on abort"
  (unless (ignore-errors ad-do-it t)
    (setq ad-return-value "q"))
  (if (and
       (equal "q" ad-return-value)
       (equal "capture" (frame-parameter nil 'name)))
      (delete-frame)))

(use-package noflet
  :ensure t )
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
		(height . 24)
		(width . 100)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(use-package org-journal
  :ensure t
  :bind (:map org-journal-mode-map
  	      ("C-c C-k" . kill-journal-frame))
  :config
  ;; Location of journal directory
  (setq org-journal-dir (org-file-path "journal/"))

  ;; Set up function for capturing a journal entry
  (defun make-journal-frame ()
    "Create a new frame and run org-journal."
    (interactive)
    (make-frame '((name . "journal")
		  (height . 24)
		  (width . 100)))
    (select-frame-by-name "journal")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-journal-new-entry nil)))

  (defun kill-journal-frame ()
    "Save buffer, kill it, and close the frame or window. 
Will attempt to close frame if in a 'journal' frame, otherwise should attempt to close the window."
    (interactive)
    (save-buffer)
    (kill-buffer)
    (unless (equal "journal" (frame-parameter nil 'name))
      (delete-window))
    (when (equal "journal" (frame-parameter nil 'name))
      (delete-frame))))

(find-file-noselect (org-file-path "index.org"))
(find-file-noselect (org-file-path "projects.org"))
(find-file-noselect (org-file-path "notes.org"))
