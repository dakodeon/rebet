;; [[file:~/.source/rebet/rebetORG.org::rebet-backbone][rebet-backbone]]
;;; Code:

(defun rebet--get-data-from-id3 (path)
  "Returns an alist based on the ID3 tags of file in PATH."
  (mapcar (lambda (x)
	    (let ((pair (split-string x "=" t "TAG:")))
	      (cons (car pair) (nth 1 pair))))
	  (seq-remove (lambda (str) (string-match-p "FORMAT" str))
		      (process-lines
		       "ffprobe" "-loglevel" "error" "-show_entries"
		       "format_tags=title,composer,artist,rhythm,dromos,tone,date"
		       (expand-file-name path)))))

(defun rebet--set-tags (path val-list)
  "Set the id3 tags of the file in PATH, using values from VAL-LIST.
       VAL-LIST should be of the form ((<tag> . <value>)).
   Uses the eyeD3 program."
  (let ((optlist (mapcar
		  (lambda (attr)
		    (cons (get attr 'eye-opt) (cdr (assoc (get attr 'tagname) val-list))))
		  (seq-remove (lambda (x)
				(not (cdr (assoc (get x 'tagname) val-list))))
			      rebet--attributes)))
	(path (expand-file-name path)))
    (shell-command (concat "eyeD3 " (shell-quote-argument path) " "
			   (mapconcat
			    (lambda (x)
			      (concat (car x) (shell-quote-argument (cdr x))))
			    optlist " ") " >/dev/null"))))

(provide 'rebet-backbone)

;;; rebet-backbone.el ends here
;; rebet-backbone ends here
