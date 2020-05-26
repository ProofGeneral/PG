;;; init-tests.el --- tests init file -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Load this file to setup MELPA and tests-related packages.
;;

;;; Code:

;; Setup MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
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
  (require 'ert-async))

;;; init-tests.el ends here
