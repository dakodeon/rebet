;;; rebet.el --- rebetika songs tags and lyrics management

;; Author: dakodeon
;; Keywords: music, dired, org-mode, organize, lyrics

;;; Commentary:

;; This package is used to manage my collection of rebetika songs. These
;; are organized in a specified directory, with well-defined id3 tags, and
;; the same naming convention derived from my personal script
;; (mp3-tag-editor). At the same time, I keep a lyrics file, written in
;; org-mode, each entry being a song with properties, which are the same as
;; the id3 tags.

;; The idea is that the lyrics file should be inter-connected with the
;; actual songs. This means the ability to find the song from the lyrics
;; and vice-versa, renaming a song title according to it's actual title id3
;; tag, passing id3 tags to org-properties and vice-versa. Of course,
;; setting the id3 tags themselves should be done in dired, with
;; autocompletion for values.

;; The id3 tags are organized as such:
;; - title: the title of the song
;; - composer: the composer of the song
;; - artist: the singer(s) of the song
;; - year: recording year
;; - rhythm: rebetiko-style rhythmic patterns (zeibekiko, tsifteteli etc)
;; - dromos: lacking of a better word, "dromos" is a form of scale, derived
;; from arabic "maquams"
;; - tone: the tonality of the song

;; All these have their equivalents in org-properties

;; This package depends on some emacs packages, as well as some external tools and a custom script.

;; Emacs dependencies: ivy, counsel, counsel-fzf, org-mode, dired
;; External dependencies: ffprobe, eyeD3, fzf
;; My script is part of my dotfiles (in the scripts directory) and can be
;; found here: https://github.com/dakodeon/dotfiles

;;; Code:

(require 'ivy)
(require 'counsel)
(require 'dired)
(require 'org)

;; variables

;;;###autoload
(defvar rebet-default-dir "~/Music/Ρεμπέτικα"
  "The default directory where songs are stored.")

(defvar rebet-dirs-list nil
  "An optional list of additional directories where rebetika are stored,
	      besides `rebet-default-dir', eg some download dir.
	      Optionally, you can include the value of
	      `rebet-default-dir` for consistency.")

;;;###autoload
(defvar rebet-lyrics-file "~/stixoi.org"
  "The default file for rebetika lyrics.
It should be an org file.")

(defvar rebet--ref-file "~/Music/Ρεμπέτικα/.ref-list"
  "Where to store the reference list used for searching and
	building autocomp lists")

;;; empty symbols (maybe we should make them nil) --- or maybe we should
;;; make them not symbols, just a list of properties lists, with a property
;;; to store temp values
(setplist 'rebet--title
	  '(tagname "title" propname "ITEM"
		    'val nil cmd-opt "-t "))
(setplist 'rebet--composer
	  '(tagname "composer" propname "ΣΥΝΘΕΤΗΣ"
		    'val nil cmd-opt "-c " autocomp-lst nil))
(setplist 'rebet--artist
	  '(tagname "artist" propname "ΤΡΑΓΟΥΔΙΣΤΗΣ"
		    'val nil cmd-opt "-a " autocomp-lst nil))
(setplist 'rebet--rhythm
	  '(tagname "rhythm" propname "ΡΥΘΜΟΣ"
		    'val nil cmd-opt "-r " autocomp-lst nil))
(setplist 'rebet--dromos
	  '(tagname "dromos" propname "ΔΡΟΜΟΣ"
		    'val nil cmd-opt "-d " autocomp-lst nil))
(setplist 'rebet--tone
	  '(tagname "tone" propname "ΤΟΝΟΣ"
		    'val nil cmd-opt "-n " autocomp-lst
		    ("Ντο" "Ντο#" "Ρε" "Ρε#" "Μι" "Φα" "Φα#" "Σολ" "Σολ#" "Λα" "Λα#" "Σι")))
(setplist 'rebet--year
	  '(tagname "date" propname "ΕΤΟΣ_ΗΧΟΓΡ"
		    'val nil cmd-opt "-y "))

;;;###autoload
(defvar rebet--attributes
  '(rebet--title rebet--composer rebet--artist
		 rebet--rhythm rebet--dromos rebet--tone rebet--year))

;; internal functions
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

(defun rebet--get-data-from-props ()
  "Returns an alist based on the org properties of the current entry.
     Makes sense to use only in the lyrics file (`rebet-lyrics-file')."
  (let ((props (org-entry-properties)))
    (mapcar (lambda (attr)
	      (cons (get attr 'tagname)
		    (cdr (assoc (get attr 'propname) props))))
	    rebet--attributes)))

(defun rebet--make-tag-cmd (path rename val-list)
  "Makes the string of the command that will set the ID3 tags of the file
in PATH. If RENAME is non-nil, the option to rename the file will be added.
VAL-LIST should be an alist similar to the output of
`rebet--get-data-from-id3', ie of the form ((\"composer\" . \"composer
name\") (\"artist\" .\"artist name\")) etc. Nil values will be ignored and
will not activate the corresponding option."
  (let ((optlist (mapcar
		  (lambda (attr)
		    (cons (get attr 'tagname) (get attr 'cmd-opt)))
		  rebet--attributes)))
    (concat "mp3-tag-editor -EX" (if rename "R " " ")
	    (mapconcat
	     (lambda (x)
	       (let ((val (cdr (assoc (car x) val-list))))
		 (if (and val (not (string-match-p "^ *$" val)))
		     (concat (cdr x) (shell-quote-argument val) " ")
		   "")))
	     optlist "")
	    (shell-quote-argument path))))

(defun rebet--from-list-to-file (lst file)
  "Build a file from a list."
  (with-temp-file (expand-file-name file)
    (insert (mapconcat 'identity lst "\n"))))

(defun rebet--set-fzf-cmd ()
  "Builds the string for the default fzf command used to search
	    for rebetika."
  (concat "FZF_DEFAULT_COMMAND=find "
	  (mapconcat 'identity
		     (delete-dups
		      (push rebet-default-dir rebet-dirs-list)) " ")
	  " -name \"*.mp3\""))

(defun rebet--make-ref-line (path var-list)
  "Creates the string to put in ref file."
  (concat "|" (mapconcat
	       (lambda (x) (concat (car x) ":" (cdr x)))
	       var-list "|")
	  "|path:" path))

(defun rebet--make-ref-file ()
  "Makes the reference file, located in `rebet--ref-file',
     according to the mp3 files in `rebet-default-dir'. It can take
     some time to complete, depending on the number of files."
  (rebet--from-list-to-file
   (mapcar
    (lambda (file)
      (message "%s" file)
      (let ((song-data (rebet--get-data-from-id3 file)))
	(rebet--make-ref-line file song-data)))
    (directory-files-recursively (expand-file-name rebet-default-dir) "mp3"))
   rebet--ref-file))

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

(defun rebet--add-line-to-ref (path var-list)
  "Adds a new line in the `rebet--ref-file', for a file with path
  PATH and the data included in VAR_LIST. First it checks for
  lines referring to the same filename (independently of the
  directory) and removes them. It will also update the
  autocomp-lists. VAR-LIST should be an alist, similar to the
  output of `rebet--get-data-from-id3'."
  (let* ((filename (file-name-base path))
	 (path (expand-file-name path))
	 (ref-file (expand-file-name rebet--ref-file)))
    (with-temp-file ref-file
      (insert-file-contents ref-file)
      (delete-matching-lines (concat "path:.*" filename "\\.mp3") (point-min) (point-max))
      (insert (concat (rebet--make-ref-line path var-list) "\n"))))
  (rebet--make-ref-lists))

;; interactive functions

;;;###autoload
(defun rebet-update-all-refs ()
  "Run to initialize reference file and variables, according to
the mp3 files in `rebet-default-dir'. Calls
`rebet--make-ref-file' and `rebet--make-ref-lists'
consecutively."
  (interactive)
  (rebet--make-ref-file)
  (rebet--make-ref-lists))

;;;###autoload
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

;;;###autoload
(defun rebet-props-to-tags (&optional path)
   "Works only in `rebet-lyrics-file'. Reads the properties from
   the current entry and converts them to ID3 tags of a selected
   mp3 file, also renaming it if necessary. If PATH is provided,
   it acts on this file, else it calls counsel-fzf."
  (interactive)
  (unless (string= buffer-file-name (expand-file-name rebet-lyrics-file))
    (user-error "Error: Not in the lyrics file (%s)" rebet-lyrics-file))
  (let* ((process-environment (cons (rebet--set-fzf-cmd)
				    process-environment))
	 (song-data (rebet--get-data-from-props))
	 (path (if path (expand-file-name path)
		 (l/counsel-fzf-as-text "Give props to: "))))
    (shell-command (rebet--make-tag-cmd path t song-data))
    (rebet--add-line-to-ref path song-data))
  (if mark-active (deactivate-mark t)))


;;;###autoload
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

;;;###autoload
(defun rebet-dired-mp3-tag-editor ()
  "Simple interface directly to the mp3-tag-editor shell script,
   using `make-term'. No completions and no updating lists etc
   with new values. Acts on the marked files of the current dired
   buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (apply 'make-term "mp3-tag-editor" "sh" nil "mp3-tag-editor" "-ER" files)
    (switch-to-buffer-other-window "*mp3-tag-editor*")))

;;;###autoload
(defun rebet-dired-set-tags ()
  (interactive)
  (rebet--make-ref-lists)
  (let ((files (dired-get-marked-files)))
    (mapc (lambda (file)
	    (let ((def-values (rebet--get-data-from-id3 file))
		  (fname (file-name-base file)))
	      (mapc (lambda (attr)
		      (let ((defval (cdr (assoc (get attr 'tagname) def-values))))
			(put attr 'val
			     (completing-read
			      (concat fname " | " (get attr 'tagname) ": ")
			      (get attr 'autocomp-lst) nil nil defval))))
		    rebet--attributes)
	      ;; use the data to make the command and also add to the ref-file
	      ))
	  files)))

(provide 'rebet)
;;; rebet.el ends here
