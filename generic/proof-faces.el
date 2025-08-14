;;; proof-faces.el --- Faces for Proof General

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Author:      David Aspinall <David.Aspinall@ed.ac.uk> and others

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; In an ideal world, faces should work sensibly:
;;
;;   a) with default colours
;;   b) with -rv
;;   c) on console
;;   d) on different Emacsen/architectures (win32, mac, etc)
;;
;; But it's difficult to keep track of all that!
;; Please report any bad/failing colour
;; combinations (with suggested improvements) at
;; https://github.com/ProofGeneral/PG/issues
;;
;; Some of these faces aren't used by default in Proof General,
;; but you can use them in font lock patterns for specific
;; script languages.

;;; Code:

(defgroup proof-faces nil
  "Faces used by Proof General."
  :group 'proof-general
  :prefix "proof-")

;; TODO: get rid of this list.  Does 'default work widely enough
;; by now?
(defconst pg-defface-window-systems
  '(x            ;; bog standard
    mswindows    ;; Windows
    w32	         ;; Windows
    gtk          ;; gtk emacs (obsolete?)
    mac          ;; used by Aquamacs
    carbon       ;; used by Carbon XEmacs
    ns           ;; NeXTstep Emacs (Emacs.app)
    x-toolkit)   ;; possible catch all (but probably not)
  "A list of possible values for variable `window-system'.
If you are on a window system and your value of variable `window-system' is
not listed here, you may not get the correct syntax colouring behaviour.")

(defmacro proof-face-specs (bl bd ow)
  "Return a spec for `defface' with BL for light background, BD for dark, OW o/w."
  `(append
    (apply 'append
     (mapcar
     (lambda (ty) (list
		     (list (list (list 'type ty) '(class color)
			   (list 'background 'light))
			   (quote ,bl))
		     (list (list (list 'type ty) '(class color)
				 (list 'background 'dark))
			   (quote ,bd))))
     pg-defface-window-systems))
    (list (list t (quote ,ow)))))

(defface proof-queue-face
  (proof-face-specs
   (:background "mistyrose" :extend t) ;; was "darksalmon" in PG 3.4,3.5
   (:background "mediumorchid" :extend t)
   (:foreground "white" :background "black" :extend t))
  "*Face for commands in proof script waiting to be processed."
  :group 'proof-faces)

(defface proof-locked-face
  (proof-face-specs
   ;; This colour is quite subjective and may be best chosen according
   ;; to the type of display you have.
   (:background "#eaf8ff" :extend t)
   (:background "darkslateblue" :extend t)
   (:underline t :extend t))
  "*Face for locked region of proof script (processed commands)."
  :group 'proof-faces)

(defface proof-declaration-name-face
  (proof-face-specs
   (:foreground "chocolate" :bold t)
   (:foreground "orange" :bold t)
   (:italic t :bold t))
  "*Face for declaration names in proof scripts.
Exactly what uses this face depends on the proof assistant."
  :group 'proof-faces)

(defface proof-tacticals-name-face
  (proof-face-specs
   (:foreground "MediumOrchid3")
   (:foreground "orchid")
   (bold t))
  "*Face for names of tacticals in proof scripts.
Exactly what uses this face depends on the proof assistant."
  :group 'proof-faces)

(defface proof-tactics-name-face
  (proof-face-specs
   (:foreground "darkblue")
   (:foreground "mediumpurple")
   (:underline t))
  "*Face for names of tactics in proof scripts.
Exactly what uses this face depends on the proof assistant."
  :group 'proof-faces)

(defface proof-error-face
  (proof-face-specs
   (:background "rosybrown1") ; a drab version of misty rose
   (:background "brown")
   (:bold t))
  "*Face for error messages from proof assistant."
  :group 'proof-faces)

(defface proof-warning-face
  (proof-face-specs
   (:background "lemon chiffon")
   (:background "orange2")
   (:italic t))
  "*Face for warning messages.
Warning messages can come from proof assistant or from Proof General itself."
  :group 'proof-faces)

(defface proof-eager-annotation-face
  (proof-face-specs
   (:background "palegoldenrod")
   (:background "darkgoldenrod")
   (:italic t))
  "*Face for important messages from proof assistant."
  :group 'proof-faces)

(defface proof-debug-message-face
  (proof-face-specs
   (:foreground "Gray65")
   (:background "Gray30")
   (:italic t))
  "*Face for debugging messages from Proof General."
  :group 'proof-faces)

(defface proof-boring-face
  (proof-face-specs
   (:foreground "Gray75")
   (:background "Gray30")
   (:italic t))
  "*Face for boring text in proof assistant output."
  :group 'proof-faces)

(defface proof-mouse-highlight-face
  (proof-face-specs
   (:background "lightblue")
   (:background "darkslateblue")
   (:italic t))
  "*General mouse highlighting face used in script buffer."
  :group 'proof-faces)

(defface proof-command-mouse-highlight-face
  (proof-face-specs
   (:background "gold1")
   (:background "gold4")
   (:italic t))
  "*Mouse highlighting face for atomic regions (usually commands) in script buffer."
  :group 'proof-faces)

(defface proof-region-mouse-highlight-face
  (proof-face-specs
   (:background "yellow2")
   (:background "yellow3")
   (:italic t))
  "*Mouse highlighting face for compound regions (usually proofs) in script buffer."
  :group 'proof-faces)

(defface proof-highlight-dependent-face
  (proof-face-specs
   (:background "orange")
   (:background "darkorange")
   (:italic t))
  "*Face for showing (backwards) dependent parts."
  :group 'proof-faces)

(defface proof-highlight-dependency-face
  (proof-face-specs
   (:background "khaki")
   (:background "peru")
   (:italic t))
  "*Face for showing (forwards) dependencies."
  :group 'proof-faces)

(defface proof-active-area-face
  (proof-face-specs
   (:background "lightyellow" :box (:line-width 2 :color "grey75" :style released-button))
   (:background "darkyellow" :underline t)
   (:underline t))
  "*Face for showing active areas (clickable regions), outside of subterm markup."
  :group 'proof-faces)

(defface proof-script-sticky-error-face
  (proof-face-specs
   (:background "indianred1")
   (:background "indianred3")
   (:underline t))
  "Proof General face for marking an error in the proof script."
  :group 'proof-faces)

(defface proof-script-highlight-error-face
  (proof-face-specs
    (:background "indianred1" :bold t)
    (:background "indianred3" :bold t)
    (:underline t :bold t))
  "Proof General face for highlighting an error in the proof script."
  :group 'proof-faces)

(defface proof-omitted-proof-face
  (proof-face-specs
   (:background "#EAEFFF" :extend t)
   (:background "#9C4A90" :extend t)
   (:foreground "white" :background "black" :extend t))
  "*Face for background of omitted proofs."
  :group 'proof-faces)


;;; Compatibility: these are required for use in GNU Emacs/font-lock-keywords

(defconst proof-face-compat-doc "Evaluates to a face name, for compatibility.")
(defconst proof-queue-face 'proof-queue-face proof-face-compat-doc)
(defconst proof-locked-face 'proof-locked-face proof-face-compat-doc)
(defconst proof-declaration-name-face 'proof-declaration-name-face proof-face-compat-doc)
(defconst proof-tacticals-name-face 'proof-tacticals-name-face proof-face-compat-doc)
(defconst proof-tactics-name-face 'proof-tactics-name-face proof-face-compat-doc)
(defconst proof-error-face 'proof-error-face proof-face-compat-doc)
(defconst proof-script-sticky-error-face 'proof-script-sticky-error-face proof-face-compat-doc)
(defconst proof-script-highlight-error-face 'proof-script-highlight-error-face proof-face-compat-doc)
(defconst proof-warning-face 'proof-warning-face proof-face-compat-doc)
(defconst proof-eager-annotation-face 'proof-eager-annotation-face proof-face-compat-doc)
(defconst proof-debug-message-face 'proof-debug-message-face proof-face-compat-doc)
(defconst proof-boring-face 'proof-boring-face proof-face-compat-doc)
(defconst proof-mouse-highlight-face 'proof-mouse-highlight-face proof-face-compat-doc)
(defconst proof-command-mouse-highlight-face 'proof-command-mouse-highlight-face proof-face-compat-doc)
(defconst proof-region-mouse-highlight-face 'proof-region-mouse-highlight-face proof-face-compat-doc)
(defconst proof-highlight-dependent-face 'proof-highlight-dependent-face proof-face-compat-doc)
(defconst proof-highlight-dependency-face 'proof-highlight-dependency-face proof-face-compat-doc)
(defconst proof-active-area-face 'proof-active-area-face proof-face-compat-doc)
(defconst proof-script-error-face 'proof-script-errror-face-compat-doc)


(provide 'proof-faces)

;;; proof-faces.el ends here
