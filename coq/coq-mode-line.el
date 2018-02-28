;;; coq-mode-line.el -- customizing the Emacs mode line for the coq-mode buffer

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel
;; Portions © Copyright 2016-2018  Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'proof-buffers)
(require 'coq-state-vars)

(defcustom coq-scripting-indicator
  '(:eval (propertize 
   " Script " 'face 
   (cond
    ((proof-with-current-buffer-if-exists proof-script-buffer
					  (proof-locked-region-full-p))
     'font-lock-type-face) ;; TODO : does this ever happen?
    (t 'mode-line))))
  "Modeline indicator for active scripting buffer.
Changes color to indicate whether locked region is full."
  :type 'sexp
  :group 'coq-modeline)

(unless
    (assq 'proof-active-buffer-fake-minor-mode minor-mode-alist)
  (setq minor-mode-alist
	(append minor-mode-alist
		(list
		 (list
		  'proof-active-buffer-fake-minor-mode
		  coq-scripting-indicator)))))

(defvar coq-modeline-string2 ")")
(defvar coq-modeline-string1 ")")
(defvar coq-modeline-string0 " Script(")

(defun coq-build-subgoals-string (n s)
  (concat coq-modeline-string0 (int-to-string n)
          "-" (if (zerop s) "" (int-to-string s))
          (if (> n 1) coq-modeline-string2
            coq-modeline-string1)))

(defun coq-update-minor-mode-alist ()
  "Modify `minor-mode-alist' to display the number of subgoals in the modeline."
  (when proof-script-buffer
    (with-current-buffer proof-script-buffer
      (let ((toclean (assq 'proof-active-buffer-fake-minor-mode minor-mode-alist)))
        (while toclean ;; clean minor-mode-alist
          (setq minor-mode-alist (remove toclean minor-mode-alist))
          (setq toclean (assq 'proof-active-buffer-fake-minor-mode minor-mode-alist)))
        (setq minor-mode-alist
              (append (list (list 'proof-active-buffer-fake-minor-mode
                                  (coq-build-subgoals-string coq-num-goals coq-num-subgoals)))
                      minor-mode-alist))))))

(defun coq-mode-line-update (fmt)
  (setq mode-line-format fmt))

(provide 'coq-mode-line)
