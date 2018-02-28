;; testlib.el

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel
;; Portions © Copyright 2016-2018  Massachusetts Institute of Technology

;; Author: Paul Steckler

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
;; library of helper code for PG+coq tests

;;; Code:

(defun use-file (file)
  (switch-to-buffer (find-file file))
  (proof-activate-scripting))

(defun process-file (file)
  (use-file file)
  (proof-process-buffer))

(defun retract-file (file)
  (use-file file)
  (proof-retract-buffer))

(defun debug-msg (msg &rest args)
  (let ((str (apply 'format msg args)))
    (princ str 'external-debugging-output)
    (princ "\n" 'external-debugging-output)))

(defun verify-response (expected)
  (with-current-buffer proof-response-buffer
    (let ((got (buffer-substring-no-properties (point-min) (point-max))))
      (unless (equal expected got)
	(debug-msg "*** Contents of response buffer are NOT correct ***")
	(debug-msg "EXPECTED: %s" expected)
	(debug-msg "GOT:      %s" got)))))

(defun pause-to-refresh (&optional msg)
  ;; let processing finish
  (sleep-for 1)
  ;; make sure we can see the script
  (redisplay)
  (when msg
    (message msg))
  ;; allow human to eyeball what's happened
  (sleep-for 3))



