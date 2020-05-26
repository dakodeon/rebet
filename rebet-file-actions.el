;; [[file:~/.source/rebet/rebetORG.org::rebet-file-actions][rebet-file-actions]]
;;; Code:

(require 'rebet-variables)
(require 'rebet-backbone)
(require 'rebet-ref)

(defun rebet--make-name-string (val-list)
  "Formats the string for file names using values in VAL-LIST.
   Naming is of the form:
   - <artist> - <title>, if <composer> is nil, or if <composer> is the same as <artist>
   - <composer> (<artist>) - <title> otherwise.
   If <title> or both <artist> and <composer> are nil, the name is also nil."
  (let* ((artist (cdr (assoc "artist" val-list)))
	 (composer (cdr (assoc "composer" val-list)))
	 (title (cdr (assoc "title" val-list)))
	 (creators (if artist
		       (if (or (not composer) (string= composer artist))
			   artist
			 (concat composer " (" artist ")")))))
    (if (and title creators)
	(concat creators " - " title))))

(defun rebet--rename (path)
  "Renames a file in PATH according to its ID3 tags.
   Renaming occurs only if the current file name is different from
   the new file name and the file has an mp3 extension. If the file is finally renamed, the entry associated with it in ref-file gets also updated.
   See `rebet--make-name-string' for the formatting of the file name."
  (let* ((path (expand-file-name path))
	 (oldname (file-name-base path))
	 (dir (file-name-directory path))
	 (ext (file-name-extension path))
	 (newname (rebet--make-name-string (rebet--get-data-from-id3 path)))
	 (newpath (concat dir newname "." ext)))

    (unless (or (not newname) (string= oldname newname))
      (rename-file path newpath)
      (rebet--ref-remove-lines path)
      (rebet-mk-ref newpath))))

(defun rebet-dired-tags ()
  "Prompts to set tags of marked files of dired buffer, also
	renaming the files if necessary. With prefix argument omits the
	renaming part. 
      Throws errors if called from non-dired buffer or if the marked files
	contain non-mp3 files."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (user-error "Error: Not in dired buffer."))
  (rebet--make-ref-lists)
  (let ((files (dired-get-marked-files)))
    (mapc (lambda (file) (unless (string= (file-name-extension file) "mp3")
			   (user-error "Error: Selection contains non-mp3 files.")))
	  files)
    (mapc (lambda (file)
	    (let* ((def-values (rebet--get-data-from-id3 file))
		   (fname (file-name-base file))
		   (new-values (delete nil (mapcar
					    (lambda (attr)
					      (let* ((tag (get attr 'tagname))
						     (defval (cdr (assoc tag def-values)))
						     (newval (completing-read
							      (concat fname " | Set " tag ": ")
							      (get attr 'autocomp-lst)
							      nil nil defval)))
						(unless (string= newval "")
						  (cons tag newval))))
					    rebet--attributes))))
	      (rebet--set-tags file new-values)
	      (unless current-prefix-arg
		(rebet--rename file))))
	  files)
    (revert-buffer)))

(defun rebet-dired-tag-to-all ()
  "Change a single ID3 tag to multiple files. Acts on the marked
files of the current dired buffer."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (user-error "Error: Not in dired buffer."))
  (rebet--make-ref-lists)
  (let* ((files (dired-get-marked-files))
	 (tag (completing-read "Which tag? "
			       (mapcar (lambda (x) (get x 'tagname)) rebet--attributes) nil t))
	 (attr (seq-find (lambda (x) (string= (get x 'tagname) tag)) rebet--attributes))
	 (val (completing-read (concat "Set " tag " value: ")
			       (get attr 'autocomp-lst))))
    (mapc (lambda (file)
	    (rebet--set-tags file (list (cons tag val)))
	    (rebet--rename file))
	  files)
    (revert-buffer)))

(defun rebet-change-tag-everywhere ()
  "Change the value of a tag for each file in ref-file."
  (interactive)
  (rebet--make-ref-lists)
  (let* ((ref-file (expand-file-name rebet--ref-file))
	 (tag (completing-read "Which tag? "
			       (mapcar (lambda (x) (get x 'tagname)) rebet--attributes) nil t))
	 (attr (seq-find (lambda (x) (string= (get x 'tagname) tag)) rebet--attributes))
	 (oldval (completing-read (concat tag " value to change: ")
				  (get attr 'autocomp-lst) nil t))
	 (newval (completing-read (concat "Change " tag " \"" oldval "\" to: ")
				  (get attr 'autocomp-lst))))
    (unless (string= oldval newval)
      (with-temp-buffer
	(insert-file-contents ref-file)
	(let ((matching (mapcar
			 (lambda (ln)
			   (replace-regexp-in-string
			    "path:" ""
			    (seq-find (lambda (x) (string-match-p "path:" x))
				      (split-string ln "|"))))
			 (seq-remove
			  (lambda (x) (not (string-match-p (concat tag ":" oldval) x)))
			  (split-string (buffer-string) "\n")))))
	  (mapc (lambda (path)
		  (if (file-exists-p path)
		      (progn
			(rebet--set-tags path (list (cons tag newval)))
			(rebet--rename path))))
		matching))))))

(provide 'rebet-file-actions)

;;; rebet-file-actions.el ends here
;; rebet-file-actions ends here
