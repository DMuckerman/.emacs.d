;; Set to the location of your Org files on your local system
(setq beancount-directory "~/Nextcloud/")
(setq beancount-file "ledger.beancount")
(setq remote-user "dan")
(setq remote-host "flamwenco.com")
(setq remote-port "222")

(defun remote-beancount-path (filename)
  "Return the remote path for the beancount file."
  (concat "/scp:" remote-user "@" remote-host "#" remote-port ":/home/" remote-user "/" filename))

(defun update-from-remote ()
  "Copy beancount file from remote to local, in case changes were made in the online editor."
  (interactive)
  (delete-file (concat beancount-directory "/" beancount-file))
  (copy-file (remote-beancount-path beancount-file) beancount-directory)
  (open-beancount-ledger))

(defun update-to-remote ()
  "Copy beancount file to remote from local."
  (interactive)
  (delete-file (remote-beancount-path beancount-file))
  (copy-file (concat beancount-directory beancount-file) (remote-beancount-path beancount-file)))

(defun revert-files (&rest files)
  "Reload all specified files from disk.
Only files that are currently visited in some buffer are reverted.
Do not ask confirmation unless the buffer is modified."
  (save-excursion
    (let ((revert-without-query '("")))
      (dolist (file-name files)
        (message "Considering whether to revert file %s" file-name)
        (let ((buf (find-buffer-visiting file-name)))
          (when buf
            (message "Reverting file in buffer %s" (buffer-name buf))
            (set-buffer buf)
	    (revert-buffer t nil t)))))))

(defun open-beancount-ledger ()
  "Open my beancount ledger."
  (interactive)
  (if (get-file-buffer "ledger.beancount")
      (revert-files (concat beancount-directory beancount-file))
    (find-file (concat beancount-directory beancount-file))))

(global-set-key (kbd "C-c b o") 'open-beancount-ledger)
(global-set-key (kbd "C-c b u") 'update-from-remote)
(global-set-key (kbd "C-c b p") 'update-to-remote)
