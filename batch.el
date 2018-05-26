(setq package-load-list '((git-auto-commit-mode t)))
(package-initialize)

(require 'cl) ; remove-if-not is inside the common-lisp package
(setq org-agenda-files (remove-if-not 'file-exists-p '("~/Nextcloud/Org/")))
