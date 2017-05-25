;; coq-parsing.el --- find ids or notation in Coq scripts

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

(require 'coq-syntax)

(defun coq-remove-trailing-dot (s)
  "Return the string S without its trailing \".\" if any.
Return nil if S is nil."
  (if (and s (string-match "\\.\\'" s))
      (substring s 0 (- (length s) 1))
    s))

(defun coq-remove-heading-quote (s)
  "Return the string S without its heading \"\'\" if any.
Return nil if S is nil."
  (if (and s (string-match "\\`'" s))
      (substring s 1 (length s))
    s))

(defun coq-clean-id-at-point (s)
  (coq-remove-heading-quote (coq-remove-trailing-dot s)))

(defun coq-is-symbol-or-punct (c)
  "Return non nil if character C is a punctuation or a symbol constituent.
If C is nil, return nil."
  (let* ((symb (cond
		((fboundp 'symbol-near-point) (symbol-near-point))
		((fboundp 'symbol-at-point) (symbol-at-point))))
	 (symbclean (when symb (coq-clean-id-at-point (symbol-name symb)))))
    (when (and symb (not (zerop (length symbclean))))
      symbclean)))

(defun coq-grab-punctuation-left (pos)
  "Return a string made of punctuations chars found immediately before position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-before currpos))
      (setq res (concat (char-to-string (char-before currpos)) res))
      (setq currpos (- currpos 1)))
    res))


(defun coq-grab-punctuation-right (pos)
  "Return a string made of punctuations chars found immediately after position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-after currpos))
      (setq res (concat res (char-to-string (char-after currpos))))
      (setq currpos (+ currpos 1)))
    res))

(defun coq-notation-at-position (pos)
  "Return the notation at current point.
Support dot.notation.of.modules."
  (coq-with-altered-syntax-table
   (when (or (coq-grab-punctuation-left pos) (coq-grab-punctuation-right pos))
     (concat (coq-grab-punctuation-left pos)
             (coq-grab-punctuation-right pos)))))

(defun coq-string-starts-with-symbol (s)
  (eq 0 (string-match "\\s_" s)))

;; remove trailing dot if any.
(defun coq-id-at-point ()
  "Return the identifier at current point.
Support dot.notation.of.modules."
  (coq-with-altered-syntax-table
   (let* ((symb (cond
                 ((fboundp 'symbol-near-point) (symbol-near-point))
                 ((fboundp 'symbol-at-point) (symbol-at-point))))
          (symbclean (when symb (coq-clean-id-at-point (symbol-name symb)))))
     (when (and symb (not (zerop (length symbclean))))
       symbclean))))

(defun coq-id-or-notation-at-point ()
  (or (coq-id-at-point)
      (let ((notation (coq-notation-at-position (point))))
	(if notation (concat "\"" notation "\"") ""))))

(provide 'coq-parsing)
