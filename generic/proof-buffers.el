;; proof-buffers.el -- extra buffers to display to user

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

(require 'pg-vars)
(require 'pg-response)
(require 'proof-config)

(defun proof-prover-set-text-representation ()
  "Adjust representation for current buffer, to match `proof-shell-unicode'."
  (unless proof-prover-unicode
    ;; Prevent interpretation of multi-byte characters.
    ;; Otherwise, chars 128-255 get remapped higher, breaking regexps
    (toggle-enable-multibyte-characters -1)))

(defun proof-prover-make-log-buffer ()
  (when proof-server-log-traffic
    (let ((logger (concat "*" (downcase proof-assistant) "-log*")))
      (setq proof-server-log-buffer (get-buffer-create logger))
      (with-current-buffer proof-server-log-buffer
	(setq buffer-read-only t)
	;; SGML mode highlights tags
	;; XML mode complains too much
	(sgml-mode)))))

(defun proof-prover-make-associated-buffers ()
  "Create the associated buffers and set buffer variables holding them."
  (let ((goals	"*goals*")
	(resp	"*response*"))

    (setq proof-goals-buffer    (get-buffer-create goals))
    (setq proof-response-buffer (get-buffer-create resp))

    (proof-prover-make-log-buffer)

    (setq pg-response-special-display-regexp
	  (proof-regexp-alt goals resp))

    (with-current-buffer proof-response-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-response))

    (with-current-buffer proof-goals-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-goals))))

(defun proof-server-set-logging (enabled)
  (setq proof-server-log-traffic enabled)
  (when enabled
    (proof-prover-make-log-buffer)))

(defun proof-server-enable-logging ()
  (interactive)
  (proof-server-set-logging t))

(defun proof-server-disable-logging ()
  (interactive)
  (proof-server-set-logging nil))

(provide 'proof-buffers)


