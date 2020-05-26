;; [[file:~/.source/rebet/rebetORG.org::rebet-variables][rebet-variables]]
;;; rebet-variables

;;; Code:

(defvar rebet-default-dir "~/Music/Ρεμπέτικα"
  "The default directory where songs are stored.")

(defvar rebet-dirs-list nil
  "An optional list of additional directories where rebetika are stored,
		    besides `rebet-default-dir', eg some download dir.
		    Optionally, you can include the value of
		    `rebet-default-dir` for consistency.")

(defvar rebet-lyrics-file "~/stixoi.org"
  "The default file for rebetika lyrics.
      It should be an org file.")

(defvar rebet--ref-file "~/Music/Ρεμπέτικα/.ref-list"
  "Where to store the reference list used for searching and
	      building autocomp lists")

;; dictionary -- NOTE: cmd-opt is deprecated
(setplist 'rebet--title
	  '(tagname "title" propname "ITEM"
		    val nil cmd-opt "-t " eye-opt "--title "))
(setplist 'rebet--composer
	  '(tagname "composer" propname "ΣΥΝΘΕΤΗΣ"
		    val nil cmd-opt "-c " eye-opt "--text-frame=TCOM:" autocomp-lst nil))
(setplist 'rebet--artist
	  '(tagname "artist" propname "ΤΡΑΓΟΥΔΙΣΤΗΣ"
		    val nil cmd-opt "-a " eye-opt "--artist " autocomp-lst nil))
(setplist 'rebet--rhythm
	  '(tagname "rhythm" propname "ΡΥΘΜΟΣ"
		    val nil cmd-opt "-r " eye-opt "--text-frame=rhythm:" autocomp-lst nil))
(setplist 'rebet--dromos
	  '(tagname "dromos" propname "ΔΡΟΜΟΣ"
		    val nil cmd-opt "-d " eye-opt "--text-frame=dromos:" autocomp-lst nil))
(setplist 'rebet--tone
	  '(tagname "tone" propname "ΤΟΝΟΣ"
		    val nil cmd-opt "-n " eye-opt "--text-frame=tone:" autocomp-lst
		    ("Ντο" "Ντο#" "Ρε" "Ρε#" "Μι" "Φα" "Φα#" "Σολ" "Σολ#" "Λα" "Λα#" "Σι")))
(setplist 'rebet--year
	  '(tagname "date" propname "ΕΤΟΣ_ΗΧΟΓΡ"
		    val nil cmd-opt "-y " eye-opt "--release-year "))

(defvar rebet--attributes
  '(rebet--title rebet--composer rebet--artist
		 rebet--rhythm rebet--dromos rebet--tone rebet--year))

(provide 'rebet-variables)
;;; rebet-varables.el ends here
;; rebet-variables ends here
