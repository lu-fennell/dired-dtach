;; TODO: there seems to be a bug on lipoa. Opening a file fails when
;; the file name is too long. It could be a problem with the
;; filesystem, though. This should be investigated.



;; TODO: make the default optional. When nil, nothing is presented by default
(defcustom dired-dtach-default-launcher "xdg-open"
  "The program used for the default suggestion of `dired-dtach-find-file'"
  )

(defcustom dired-dtach-terminal-command "urxvt"
  "The terminal program used by `dired-dtach-open-terminal'"
  )

(defcustom dired-dtach-launcher-list '("xdg-open")
  "A list of programs that spawn by themselves and also not work with dtach")

(defun dired-dtach-launcher-p (program)
  (let ((trimmed-program
	 (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" program))))
    (member trimmed-program dired-dtach-launcher-list)))


;; TODO: use url-encode-url for protecting the filename... subst. of
;; `/' is still needed
;; TODO: write some tests
(defun dired-dtach-command-format-fname (file)
  (let* ((result (if (file-name-absolute-p file) (substring file 1) file))
	 (result (directory-file-name result))
	 (result (concat result "-"))
	 (result (replace-regexp-in-string "/" "__" result)))
    result))

;; TODO: factor out the `files' argument and just take a prefix for the tmpfile
;; TODO: test
(defun dired-dtach-command (files cmd)
  "Construct a shell command using the `dtach'; the program CMD
should be spawned with input files FILES (a list). A fresh
control socket will be used that somehow reflects CMD and FILES."
  (let* ((basedir (file-name-as-directory temporary-file-directory))
	 (esc-files (mapcar 'dired-dtach-command-format-fname files))
	 (socket-name (make-temp-name (eval `(concat ,@esc-files))))
        )
    (concat "dtach -n '" basedir "dtach-" socket-name "' " cmd)))

(defun dired-dtach-open-terminal ()
  (interactive)
  (dired-smart-shell-command
   (dired-dtach-command
    `(,(file-name-nondirectory (dired-current-directory)) "terminal")
    dired-dtach-terminal-command)))

(defun dired-dtach-find-file (&optional arg)
  (interactive "P")
  (cond
   (arg (dired-find-file))
   (t (let ((file (dired-get-file-for-visit)))
	;; TODO: allow customizable escape hatches
	;; - with a regex
	;; - or with a function
        (if (file-directory-p file) (dired-find-file)
          (let* ((file-list `(,file))
		 (program
		  (let ((dired-guess-shell-alist-user ;; set the default prompt
			 `((".*"  dired-dtach-default-launcher))))
		    (dired-read-shell-command "Open %s with: " nil file-list)));; TODO ask the user
		(cmd (dired-shell-stuff-it (if (dired-dtach-launcher-p program) ;; some programs spawn on their own (and do not work with dtach)
					       program
					     (dired-dtach-command file-list program))
					   file-list
					   nil ;; TODO: this is the `on-each' argument. Really nil?
					   )))
	    (message "Spawning `%s'" cmd)
            (dired-run-shell-command cmd)))))))
  
(provide 'dired-dtach)
