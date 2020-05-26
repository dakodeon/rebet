;; [[file:~/.source/rebet/rebetORG.org::rebet-ref][rebet-ref]]
;;; Code:

(require 'rebet-variables)
(require 'rebet-backbone)

(defun rebet--ref-get-path (ln)
  (if (string-match-p "path" ln)
      (replace-regexp-in-string "path:" ""
				(seq-find (lambda (x) (string-match-p "path:" x))
					  (split-string ln "|" t)))))

(defun rebet--ref-line-format (path var-list)
  "Formats the string to put in ref file."
  (concat "|" (mapconcat
	       (lambda (x) (concat (car x) ":" (cdr x)))
	       var-list "|")
	  "|path:" path))

(defun rebet--ref-remove-lines (match)
  "Remove lines containing MATCH from ref-file."
  (let ((ref-file (expand-file-name rebet--ref-file)))
    (unless (file-exists-p ref-file)
      (user-error "Error: the reference file was not found (%s)" ref-file))
    (with-temp-file ref-file
      (insert-file-contents ref-file)
      (flush-lines match (point-min) (point-max)))))

(defun rebet-ref-remove-nonexistent ()
  "Removes entries with non-existent paths from the reference file."
  (interactive)
  (let ((ref-file (expand-file-name rebet--ref-file)))
    (unless (file-exists-p ref-file)
      (user-error "Error: the reference file was not found (%s)" ref-file))
    (with-temp-file ref-file
      (insert-file-contents ref-file)
      (mapc (lambda (ln)
	      (let ((path (rebet--ref-get-path ln)))
		(message "%s" path)
		(if path (unless (file-exists-p path)
			   (flush-lines path (point-min) (point-max))))))
	    (split-string (buffer-string) "\n")))))

(defun rebet-mk-ref (&optional path)
  "Writes lines to `rebet--ref-file'. If PATH is not set, it will
  act on every mp3 file in `rebet-default-dir' recursively. If
  PATH is a directory, it will act in all .mp3 files in there,
  non-recursively. If PATH corresponds to a single file, it will
  act only on this file. With a prefix argument it replaces the
  file with the new content.

For the exact line formatting, see `rebet--ref-line-format'"
  (interactive)
  (let ((ref-file (expand-file-name rebet--ref-file))
	(path (if (not path)
		  (directory-files-recursively (expand-file-name rebet-default-dir) "mp3")
		(if (file-directory-p path)
		    (directory-files (expand-file-name path) t "mp3")
		  (if (file-exists-p path)
		      (list (expand-file-name path)))))))
    (if current-prefix-arg (delete-file ref-file))
    (if path
	(with-temp-file ref-file
	  (if (file-exists-p ref-file) (insert-file-contents ref-file))
	  (mapc (lambda (file)
		  (message "%s" file)
		  (flush-lines file (point-min) (point-max))
		  (insert
		   (concat (rebet--ref-line-format file (rebet--get-data-from-id3 file)) "\n")))
		path)))))

(defun rebet--make-ref-lists ()
  "Makes the reference lists according to the contents of
	  `rebet--ref-file'."
  (unless (file-exists-p rebet--ref-file)
    (user-error
     "ERROR: The file \"%s\" does not exist. Run M-x rebet-update-all-refs"
     rebet--ref-file))
  (with-temp-buffer
    (insert-file-contents (expand-file-name rebet--ref-file))
    (mapc (lambda (attr)
	    (put attr 'autocomp-lst
		 (remove
		  ""
		  (delete-dups
		   (mapcar
		    (lambda (ln)
		      (let ((rgx (concat "^.*" (get attr 'tagname) ":\\||.*$")))
			(replace-regexp-in-string rgx "" ln)))
		    (split-string (buffer-string) "\n"))))))
	  (seq-remove (lambda (x)
			(or
			 (not (member 'autocomp-lst (symbol-plist x)))
			 (string= (get x 'tagname) "tone")))
		      rebet--attributes))))

(provide 'rebet-ref)

;;; rebet-ref.el ends here
;; rebet-ref ends here
