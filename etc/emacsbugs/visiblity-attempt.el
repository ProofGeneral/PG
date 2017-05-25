;;; visibility-attempt.el

;;; === Test area for invisibility ===
;;;

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel and École Normale Supérieure de Lyon
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

(defvar vis nil)

(overlay-put (make-overlay 18 22) 'invisible 'smaller)
(overlay-put (make-overlay 9 43) 'invisible 'larger)

(defun toggle-invis ()
  (interactive)
  (if vis 
      (add-to-invisibility-spec '(larger . t))
    (remove-from-invisibility-spec '(larger . t)))
  (setq vis (not vis)))


;; In this buffer:

;;    M-x eval-buffer RET
;;    M-x toggle-invis

;; The smaller area remains visible, although there is a surrounding
;; overlay which has an invisibility spec which should cover the
;; revealed characters.  Arguably a bug.






