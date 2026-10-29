;;; coq-test-response-after-show.el --- Test messages displayed around the automatic Show
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; When Coq runs silently, Proof General sends a Show command after
;; the last command of the queue to update the goals.  Test that this
;; Show does not erase the response of the command before it (urgent
;; or not), that messages about the state of the proof (subproof or
;; proof complete, goals given up) are displayed in the goals buffer,
;; and that message tags (<infomsg>, <warning>) are never displayed.
;;
;; All tests process their script one command at a time, which is the
;; case where the automatic Show follows each command.

;;; Code:

;; all tests in this file run in two-pane mode
(setq proof-three-window-enable nil)
;; Some Emacs versions run with a frame width of 10 in batch mode
;; inside the container. The line breaks make some regular expressions
;; fail, therfore disable printing width adapting.
(setq coq-auto-adapt-printing-width nil)

(require 'subr-x)
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)

(message "response after Show tests run with Coq version %s" (coq-version t))


;;; utility functions

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defun buffer-text (buffer)
  "Return the content of BUFFER without surrounding blanks."
  (with-current-buffer buffer
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun should-have-no-tags (buffer)
  "Check that BUFFER does not display any message tag."
  (should-not (string-match-p "</?\\(?:infomsg\\|warning\\)>"
                              (buffer-text buffer))))

(defun check-after-marker (marker check)
  "Process response_after_show.v one command at a time up to MARKER.
Then call CHECK.
MARKER is a regular expression matching the comment following the last
command to process.  CHECK is called with the content of the goals and
the response buffers.  Tags are checked in both buffers."
  (message "Check response and goals after %s." marker)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "response_after_show.v")
          (setq buffer (current-buffer))
          (goto-char (point-min))
          (should (re-search-forward marker nil t))
          ;; end of the command before the marker comment
          (let ((target (progn (goto-char (match-beginning 0))
                               (skip-chars-backward " \t")
                               (point))))
            (while (< (proof-unprocessed-begin) target)
              (goto-char (proof-unprocessed-begin))
              (proof-assert-next-command-interactive)
              (wait-for-coq)))
          (should-have-no-tags proof-goals-buffer)
          (should-have-no-tags proof-response-buffer)
          (funcall check
                   (buffer-text proof-goals-buffer)
                   (buffer-text proof-response-buffer)))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))


;;; tests

(ert-deftest response-after-show-tactic-message ()
  "The message of a tactic is kept after the automatic Show."
  (check-after-marker
   "(\\* marker hello \\*)"
   (lambda (goals response)
     (should (equal response "hello"))
     (should (string-match-p "\\`1 \\(?:sub\\)?goal" goals)))))

(ert-deftest response-after-show-urgent-warning ()
  "A warning (urgent message) is kept after the automatic Show."
  (check-after-marker
   "(\\* marker warning \\*)"
   (lambda (goals response)
     (should (string-match-p "Tactic my_tac is deprecated since 1.0" response))
     (should (string-match-p "\\`1 \\(?:sub\\)?goal" goals)))))

(ert-deftest response-after-show-subproof-complete ()
  "The end of subproof message is displayed in the goals buffer.
The response of the command that completed the subproof is kept."
  (check-after-marker
   "(\\* marker subproof \\*)"
   (lambda (goals response)
     (should (string-match-p "\\`Finished transaction" response))
     (should (string-match-p "\\`This subproof is complete" goals))
     (should (string-match-p "^\\(?:sub\\)?goal 1 (ID [0-9]+) is:" goals)))))

(ert-deftest response-after-show-goals-given-up ()
  "The message about given up goals is displayed in the goals buffer."
  (check-after-marker
   "(\\* marker given up \\*)"
   (lambda (goals response)
     (should (string-match-p "\\`Finished transaction" response))
     (should (string-match-p
              "\\`No more \\(?:sub\\)?goals, but there are some goals you gave up"
              goals)))))

(ert-deftest response-after-show-no-more-goals ()
  "The end of proof message is displayed in the goals buffer.
The response of the command that completed the proof is kept."
  (check-after-marker
   "(\\* marker no more goals \\*)"
   (lambda (goals response)
     (should (string-match-p "\\`Finished transaction" response))
     (should (string-match-p "\\`No more \\(?:sub\\)?goals\\.\\'" goals)))))

(provide 'coq-test-response-after-show)

;;; coq-test-response-after-show.el ends here
