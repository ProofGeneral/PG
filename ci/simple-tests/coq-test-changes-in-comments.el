;;; coq-test-changes-in-comments.el --- Test changes inside comments
;;
;; This file is part of Proof General.
;; 
;; © Copyright 2025  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test that for various changes inside comments and spanning from one
;; to the next comment the buffer is correctly retracted.



;;; Code:

(defconst coq-src-comment
  "(*
  Comment 1
 *)

Definition aXXa := 1.

(*
  Comment 2

 *)

Definition bXXb := 2.
"
  "Coq source code for comment tests.")


;;; utility functions

(defun test-goto-line (line)
  "Put point on start of line LINE.
Very similar to `goto-line', but the documentation of `goto-line'
says, programs should use this piece of code."
  (goto-char (point-min))
  (forward-line (1- line))
  (cl-assert (eq (line-number-at-pos) line) nil
             "point not at required line in test-goto-line"))

(defun record-buffer-content (buf)
  "Record buffer content of BUF via `message' for debugging.
BUF should be a buffer as string or buffer object."
  (with-current-buffer buf
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (message "%s buffer contains %d chars: %s" buf (length content) content))))

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    ;; (message "wait for coq/compilation with %d items queued\n"
    ;;          (length proof-action-list))
    ;;
    ;; accept-process-output without timeout returns rather quickly,
    ;; apparently most times without process output or any other event
    ;; to process.
    (accept-process-output nil 0.1)))

(defun point-should-be-unlocked ()
  "Check that point is unlocked.
This is a simplified version of `cct-check-locked' that only
works correctly when the `proof-locked-span' does exist in the
current buffer."
  (should (and proof-locked-span
               (span-end proof-locked-span)
               (< (span-end proof-locked-span) (point)))))
          

;;; define the tests

(ert-deftest insert-only-comment-end-in-processed-comment ()
  :expected-result :failed
  "Test inserting a comment end marker inside a processed comment."
  (message "Check retract after inserting `*)' in a processed comment")

  ;; Before Emacs 27 `self-insert-command' is lacking the optional
  ;; character argument, making it too complicated to simulate typing.
  ;; When done manually, this test also fails in Emacs 26.3.
  (skip-unless (version<= "27" emacs-version))

  (let (buffer)
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (insert coq-src-comment)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))
          
          ;; insert comment end via keyboard simulation
          (test-goto-line 9)
          (self-insert-command 1 ?*)
          (self-insert-command 1 41)    ; ?) is 41, but ?) distorts indentation

          ;; check that point is not in locked region
          (forward-line 1)
          ;; (record-buffer-content (current-buffer))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest insert-comment-end-and-char-in-processed-comment ()
  "Insert a comment end marker and something else inside a processed comment."
  (message "Check retract after inserting `*)a' in a processed comment")

  ;; Before Emacs 27 `self-insert-command' is lacking the optional
  ;; character argument, making it too complicated to simulate typing.
  ;; When done manually, this test also succeeds in Emacs 26.3.
  (skip-unless (version<= "27" emacs-version))

  (let (buffer)
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (insert coq-src-comment)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))
          
          ;; insert comment end via keyboard simulation
          (test-goto-line 9)
          (self-insert-command 1 ?*)
          (self-insert-command 1 41)    ; ?) is 41, but ?) distorts indentation
          (self-insert-command 1 ?a)

          ;; check that point is not in locked region
          (forward-line 1)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest comment-definition-process-undo ()
  :expected-result (if (version<= "27" emacs-version) :failed :passed)
  "Comment a definition with `comment-dwim', process and then undo.
Reproduces issue #800. The undo should cause a retract."
  (message "Check undo of a comment region in a processed region")
  (let (buffer
        (saved-comment-style comment-style))
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (setq comment-style 'extra-line)
          (transient-mark-mode 1)

          ;; Insert `coq-src-comment' with many undo boundaries in
          ;; between. If there are too few undo boundaries (or none)
          ;; issue #800 cannot be reproduced.
          (mapc
           (lambda (s)
             (insert s)
             (insert "\n")
             (undo-boundary))
           (split-string coq-src-comment "\n"))

          ;; Comment definition aXXa
          (goto-char (point-min))
          (should (search-forward "Definition aXXa" nil t))
          (beginning-of-line)
          (set-mark (point))
          (end-of-line)
          (comment-dwim nil)
          (undo-boundary)
          ;; (record-buffer-content (current-buffer))

          ;; The mark seems to stay active - without deactivating it,
          ;; the undo won't clear the comment.
          (deactivate-mark t)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))

          ;; undo
          (pg-protected-undo nil)

          ;; Definition aXXa should not be locked
          (goto-char (point-min))
          (should (search-forward "Definition aXXa" nil t))
          (beginning-of-line)
          (should (looking-at "Definition"))
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (setq comment-style saved-comment-style))
        (kill-buffer buffer)))))

(ert-deftest yank-comment-end-in-processed-comment ()
  :expected-result :failed
  "Insert a comment end marker inside a processed comment via yank."
  (message "Check retract after yanking `*) XXX (*' in a processed comment")
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (insert coq-src-comment)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))
          
          ;; insert comment end via keyboard simulation
          (test-goto-line 9)
          ;; fake some kill
          (kill-new "*) XXX (*")
          (yank)

          ;; check that point is not in locked region
          (forward-line 1)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest kill-from-comment-to-next-comment ()
  :expected-result :failed
  "Kill from comment to comment, including some non-commented material."
  (message "Kill from comment to next comment")
  (let (buffer start end)
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (insert coq-src-comment)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))

          ;; Kill from comment one over aXXa to comment 2
          (goto-char (point-min))
          (should (search-forward "Comment 1" nil t))
          (forward-line 1)
          (setq start (point))
          (should (search-forward "Comment 2" nil t))
          (forward-line 1)
          (setq end (point))
          (kill-region start end)

          ;; check that point is not in locked region
          (forward-line 1)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest undo-breaks-up-nested-comment ()
  :expected-result :failed
  "Undo start and end of two nested comments to break up comment.
Insert a comment start marker inside first comment and a comment
end marker in second comment, such that there is one comment
covering the definition of aXXa with two nested comments. Then,
after processing, undo twice such that aXXa is outside any
comment."
  (message "Undo twice to break up comment with nested comments")
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (insert coq-src-comment)
          (undo-boundary)

          ;; insert comment start in first comment
          (goto-char (point-min))
          (should (search-forward "Comment 1" nil t))
          (forward-line 1)
          (insert "(* ")
          (undo-boundary)

          ;; insert comment end in second comment
          (should (search-forward "Comment 2" nil t))
          (forward-line -1)
          (end-of-line)
          (insert " *)")
          (undo-boundary)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))

          ;; undo twice
          (pg-protected-undo nil)
          (pg-protected-undo nil)

          ;; check that bXXb is not in locked region
          (should (search-backward "Definition aXXa" nil t))
          (beginning-of-line)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest undo-comment-spanning-over-two-comments ()
  :expected-result :failed
  "Undo a comment such that some outer comment is broken up.
Insert a new comment starting inside comment 1 and ending inside
comment 2 such that the definition aXXa is inside a comment. Then
process and undo. Requires `comment-style' to be set."
  (message "Undo a comment such that some outer comment is broken up")
  (let (buffer
        (saved-comment-style comment-style))
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (setq comment-style 'extra-line)
          (transient-mark-mode 1)

          ;; Insert `coq-src-comment' with many undo boundaries in
          ;; between.
          (mapc
           (lambda (s)
             (insert s)
             (insert "\n")
             (undo-boundary))
           (split-string coq-src-comment "\n"))

          ;; insert a new comment from comment 1 to comment 2
          (goto-char (point-min))
          (should (search-forward "Comment 1" nil t))
          (forward-line 1)
          (set-mark (point))
          (should (search-forward "Comment 2" nil t))
          (forward-line 1)
          (comment-dwim nil)
          (undo-boundary)
          (deactivate-mark t)

          ;; delete last empty line
          (goto-char (point-max))
          (backward-delete-char 1)

          ;; process
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))

          ;; undo twice
          (pg-protected-undo nil)
          (pg-protected-undo nil)

          ;; check that bXXb is not in locked region
          (should (search-backward "Definition aXXa" nil t))
          (beginning-of-line)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (setq comment-style saved-comment-style))
        (kill-buffer buffer)))))

(ert-deftest undo-kill-from-comment-to-comment ()
  :expected-result :failed
  "Kill from comment to next comment, including other material, then undo.
Kill from inside comment 1 to inside comment 2 such that the
definition aXXa is gone. Then process and undo. Requires
`comment-style' to be set."
  (message "Kill from comment to next comment, including material in between")
  (let (buffer start
        (saved-comment-style comment-style))
    (unwind-protect
        (progn
          (find-file "comments.v")
          (setq buffer (current-buffer))
          (setq comment-style 'extra-line)
          (transient-mark-mode 1)
          (insert coq-src-comment)
          (undo-boundary)

          ;; kill from comment 1 to comment 2
          (goto-char (point-min))
          (should (search-forward "Comment 1" nil t))
          (forward-line 1)
          (setq start (point))
          (should (search-forward "Comment 2" nil t))
          (beginning-of-line)
          (kill-region start (point))
          (undo-boundary)

          ;; process
          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))

          ;; check that Definition bXXb has been processed
          ;; (record-buffer-content "*coq*")
          (with-current-buffer proof-shell-buffer
            (goto-char (point-max))
            (should (search-backward "Definition bXXb" nil t)))

          ;; undo kill-region
          (pg-protected-undo nil)

          ;; check that bXXb is not in locked region
          (should (search-backward "Definition aXXa" nil t))
          (beginning-of-line)
          ;; (record-buffer-content (current-buffer))
          ;; (message "locked until %s, point at %s"
          ;;          (span-end proof-locked-span) (point))
          (point-should-be-unlocked))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (setq comment-style saved-comment-style))
        (kill-buffer buffer)))))

(provide 'coq-test-changes-in-comments)

;;; coq-test-changes-in-comments.el ends here
