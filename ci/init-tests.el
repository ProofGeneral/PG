;;; init-tests.el --- tests init file -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Load this file to setup MELPA and tests-related packages.
;;

;;; Code:

;; FIXME: Merely loading a file should not have such side effects.
;; We should move all of that code into a function.

;; Setup MELPA
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Optionally: bootstrap use-package for declarative package specs
;(unless (package-installed-p 'use-package)
;(package-refresh-contents)
;(package-install 'use-package))
;(eval-when-compile
;  (require 'use-package))

;; Bootstrap ert-async
(unless (package-installed-p 'ert-async)
  (package-refresh-contents)
  (package-install 'ert-async))

(eval-when-compile
  ;; FIXME: Why do we have this `require' and why is it within
  ;; an `eval-when-compile'?
  (require 'ert-async nil t))

;;; init-tests.el ends here
