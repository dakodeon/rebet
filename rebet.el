;; [[file:~/.source/rebet/rebetORG.org::rebet][rebet]]
;;; rebet.el -- manage mp3 collections with ID3 tags

;; author: dakodeon

;;; Commentary:

;; Provides a consistent interface to manage the ID3 tags of a music
;; collection, while also keeping an org mode lyrics file, in which the
;; entries are linked to their corresponding mp3 files.

;;; Code

(require 'dired)
(require 'org)
(require 'counsel)

(require 'rebet-variables)
(require 'rebet-backbone)
(require 'rebet-ref)
(require 'rebet-file-actions)
(require 'rebet-stray)

(rebet--make-ref-lists)

(defun rebet-init ()
  "Does nothing for now..."
  (message "Rebet power!"))

(provide 'rebet)

;;; rebet.el ends here
;; rebet ends here
