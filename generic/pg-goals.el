;; pg-goals.el ---  Proof General goals buffer mode.

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

;;; Code:
(eval-when-compile
  (require 'easymenu)			; easy-menu-add, etc
  (require 'span)			; span-*
  (defvar proof-goals-mode-menu)	; defined by macro below
  (defvar proof-assistant-menu))	; defined by macro in proof-menu

(require 'pg-assoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goals buffer mode
;;

;;;###autload
(define-derived-mode proof-goals-mode proof-universal-keys-only-mode
  proof-general-name
  "Mode for goals display.
May enable proof-by-pointing or similar features.
\\{proof-goals-mode-map}"
  (setq proof-buffer-type 'goals)
  (add-hook 'kill-buffer-hook 'pg-save-from-death nil t)
  (easy-menu-add proof-goals-mode-menu proof-goals-mode-map)
  (easy-menu-add proof-assistant-menu proof-goals-mode-map)
  (proof-toolbar-setup)
  (buffer-disable-undo)
  (if proof-keep-response-history (bufhist-mode)) ; history for contents
  (set-buffer-modified-p nil)
  (setq cursor-in-non-selected-windows nil))

;;
;; Menu for goals buffer
;;
(proof-eval-when-ready-for-assistant ; proof-aux-menu depends on <PA>
    (easy-menu-define proof-goals-mode-menu
      proof-goals-mode-map
      "Menu for Proof General goals buffer."
      (proof-aux-menu)))

;;
;; Keys for goals buffer
;;
(define-key proof-goals-mode-map [q] 'bury-buffer)
;; TODO: use standard Emacs button behaviour here (cf Info mode)
(define-key proof-goals-mode-map [mouse-1] 'pg-goals-button-action)
(define-key proof-goals-mode-map [C-M-mouse-3]
  'proof-undo-and-delete-last-successful-command)



;;
;; The completion of init
;;
;;;###autoload
(defun proof-goals-config-done ()
  "Initialise the goals buffer after the child has been configured."
  (setq font-lock-defaults '(proof-goals-font-lock-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goals buffer processing
;;
(defun pg-goals-display (string keepresponse)
  "Display STRING in the `proof-goals-buffer', properly marked up.
Converts term substructure markup into mouse-highlighted extents.

The response buffer may be cleared to avoid confusing the user
with output associated with a previous goals message.  This
function tries to do that by calling `pg-response-maybe-erase'.

If KEEPRESPONSE is non-nil, we assume that a response message
corresponding to this goals message has already been displayed
before this goals message (see `proof-shell-handle-delayed-output'),  
so the response buffer should not be cleared."
  (when proof-goals-buffer
    (with-current-buffer proof-goals-buffer
      ;; Response buffer may be out of date. It may contain (error)
      ;; messages relating to earlier proof states

      ;; Erase the response buffer if need be, maybe removing the
      ;; window.  Indicate it should be erased before the next output.

      (pg-response-maybe-erase t t nil keepresponse)

      ;; Erase the goals buffer and add in the new string

      (setq buffer-read-only nil)

      (unless (eq 0 (buffer-size))
	(bufhist-checkpoint-and-erase))

      ;; Only display if string is non-empty.
      (unless (string-equal string "")
	(insert string))

      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      
      ;; Keep point at the start of the buffer.
      (proof-display-and-keep-buffer
       proof-goals-buffer (point-min)))))

;;
;; Actions in the goals buffer
;;

(defun pg-goals-button-action (event)
  "Construct a command based on the mouse-click EVENT."
  (interactive "e")
  (let* ((posn     (event-start event))
	 (pos      (posn-point posn))
	 (buf      (window-buffer (posn-window posn)))
	 (props    (text-properties-at pos buf))
	 (sendback (plist-get props 'sendback)))
    (cond
     (sendback
      (with-current-buffer buf
	(let* ((cmdstart (previous-single-property-change pos 'sendback
							  nil (point-min)))
	       (cmdend   (next-single-property-change pos 'sendback
						      nil (point-max)))
	       (cmd      (buffer-substring-no-properties cmdstart cmdend)))
	  (proof-insert-sendback-command cmd)))))))





(provide 'pg-goals)

;;; pg-goals.el ends here
