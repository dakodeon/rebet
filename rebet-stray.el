;; [[file:~/.source/rebet/rebetORG.org::rebet-stray][rebet-stray]]
;;; Code

(require 'rebet-variables)
(require 'rebet-backbone)

;; this function will be used when searching for a song file. It
;; temporarily sets the fzf command to point to the directories containing
;; the files.
(defun rebet--set-fzf-cmd ()
  "Builds the string for the default fzf command used to search
		 for rebetika."
  (concat "FZF_DEFAULT_COMMAND=find "
	  (mapconcat 'identity
		     (delete-dups
		      (push rebet-default-dir rebet-dirs-list)) " ")
	  " -name \"*.mp3\""))

;;; the next 2 functions belong to the yet-to-be section about the lyrics file. I still have to write the props-to-tags function
(defun rebet-tags-to-props (&optional path)
  "Works only in `rebet-lyrics-file'. Reads ID3 tags from a
	selected mp3 file and converts them to org-properties in the
	entry at point. It also updates the heading, in case it
	differs from the actual title. If PATH is specified, it reads
	from this file, else it calls counsel-fzf."
  (interactive)
  (unless (string= buffer-file-name (expand-file-name rebet-lyrics-file))
    (user-error "Error: Not in the lyrics file (%s)" rebet-lyrics-file))
  (let* ((process-environment (cons (rebet--set-fzf-cmd)
				    process-environment))
	 (path (if path (expand-file-name path)
		 (l/counsel-fzf-as-text "Get props from: ")))
	 (song-data (rebet--get-data-from-id3 path))
	 (entry-data (org-entry-properties)))
    (mapc (lambda (attr)
	    (org-set-property
	     (get attr 'propname) (cdr (assoc (get attr 'tagname) song-data))))
	  (seq-remove (lambda (x) (string= (get x 'tagname) "title"))
		      rebet--attributes))
    (mapc (lambda (x)
	    (if (string-match-p "^ *$" (cdr (assoc (get x 'propname)
						   (org-entry-properties))))
		(org-delete-property (get x 'propname))))
	  rebet--attributes)
    (org-edit-headline (cdr (assoc "title" song-data))))
  ;; do not update data -- thought: maybe use rebet-autocompletion?
  (if mark-active (deactivate-mark t)))

(defun rebet-tags-to-props-and-back (&optional path)
  "Sequentially calls `rebet-tags-to-props' and
	`rebet-org-to-tags' on a specified file. If PATH is set it
	acts on this file, if not, it calls counsel-fzf."
  (interactive)
  (let* ((process-environment
	  (cons (rebet--set-fzf-cmd) process-environment))
	 (path (if path (expand-file-name path)
		 (l/counsel-fzf-as-text "Props come and go :"))))
    (rebet-tags-to-props path)
    (rebet-props-to-tags path)))

;; this function provides a simple interface to my shell script, which is no longer used. It is kept as legacy.
(defun rebet-dired-mp3-tag-editor ()
  "Simple interface directly to the mp3-tag-editor shell script,
	using `make-term'. No completions and no updating lists etc
	with new values. Acts on the marked files of the current dired
	buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (apply 'make-term "mp3-tag-editor" "sh" nil "mp3-tag-editor" "-ER" files)
    (switch-to-buffer-other-window "*mp3-tag-editor*")))

(provide 'rebet-stray)

;;; rebet-stray.el ends here
;; rebet-stray ends here
