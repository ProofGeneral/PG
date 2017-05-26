;;; pg-assoc.el --- Functions for associated buffers

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
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

;;; Commentary:
;;
;; Defines an empty mode inherited by modes of the associated buffers.
;; 

;;; Code:

(require 'proof-utils)

(define-derived-mode proof-universal-keys-only-mode fundamental-mode
  proof-general-name "Universal keymaps only"
  ;; Doesn't seem to supress TAB, RET
  (suppress-keymap proof-universal-keys-only-mode-map 'all)
  (proof-define-keys proof-universal-keys-only-mode-map
                     proof-universal-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Return a list of associated buffers
;;

;;;###autoload
(defun proof-associated-buffers ()
  "Return a list of the associated buffers.
Some may be dead/nil."
  (list proof-goals-buffer
	proof-response-buffer))

;;;###autoload
(defun proof-associated-windows (&optional all-frames)
  "Return a list of the associated buffers windows.
Dead or nil buffers are not represented in the list. Optional
argument ALL-FRAMES has the same meaning than for
`get-buffer-window'."
  (let ((bufs (proof-associated-buffers))
	buf wins)
    (while bufs
      (setq buf (car bufs))
      (if (and buf (get-buffer-window-list buf nil all-frames))
	  (setq wins (append (get-buffer-window-list buf nil all-frames) wins)))
      (setq bufs (cdr bufs)))
    wins))


(defun proof-associated-buffer-p (b) (member b (proof-associated-buffers)))


(defun proof-filter-associated-windows (lw)
  "Remove windows of LW not displaying at least one associated buffer."
  (cl-remove-if-not (lambda (w) (proof-associated-buffer-p (window-buffer w))) lw))


;;;###autoload
(defun proof-associated-frames ()
  "Return the list of frames displaying at least one associated buffer."
  (cl-remove-if-not (lambda (f) (proof-filter-associated-windows (window-list f)))
		 (frame-list)))

(provide 'pg-assoc)
;;; pg-assoc.el ends here
