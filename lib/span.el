;;; -*- lexical-binding: t -*-

;;; span.el --- Datatype of "spans" for Proof General
;;
;; Copyright (C) 1998-2009 LFCS Edinburgh
;; Author:      Healfdene Goguen
;; Maintainer:  David Aspinall <David.Aspinall@ed.ac.uk>
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
;;
;;; Commentary:
;;
;; Spans are our abstraction of extents/overlays.  Nowadays
;; we implement them directly with overlays.
;;
;; FIXME: eliminate aliases and directly use overlays
;;

;;; Code:

;; FSF says not to use cl at runtime, but OK at compile-time
;; Here, we do so to get lexical-let.
(eval-when-compile (require 'cl))       

(defalias 'span-start 'overlay-start)
(defalias 'span-end 'overlay-end)
(defalias 'span-set-property 'overlay-put)
(defalias 'span-property 'overlay-get)
(defalias 'span-properties 'overlay-properties)
(defun span-make (&rest args)
  (let ((span (apply #'make-overlay args)))
    (span-set-property span 'pg-span t)
    span))
(defalias 'span-detach 'delete-overlay)
(defalias 'span-set-endpoints 'move-overlay)
(defalias 'span-buffer 'overlay-buffer)
(defalias 'spans-in 'overlays-in)

(defun span-p (ol)
  "Check if an overlay belongs to PG."
  (overlay-get ol 'pg-span))

(defun span-read-only-hook (_overlay _after _start _end &optional _len)
  (unless inhibit-read-only
    (error "Region is read-only")))

(add-to-list 'debug-ignored-errors "Region is read-only")

(defun span-read-only (span)
  "Set SPAN to be read only."
  ;; Note: using the standard 'read-only property does not work.
  ;; (overlay-put span 'read-only t))
  (span-set-property span 'modification-hooks '(span-read-only-hook))
  (span-set-property span 'insert-in-front-hooks '(span-read-only-hook)))

(defun span-make-read-only-hook-with-predicate (pred)
  (lambda (overlay after start end &optional len)
    (when (funcall pred start end)
      (span-read-only-hook overlay after start end len))))
  
(defun span-read-only-subject-to-predicate (span pred) 
  "Set SPAN to be read only."
  (let ((hooks `(,(span-make-read-only-hook-with-predicate pred))))
    (span-set-property span 'modification-hooks hooks)
    (span-set-property span 'insert-in-front-hooks hooks)))

(defun span-read-write (span)
  "Set SPAN to be writeable."
  (span-set-property span 'modification-hooks nil)
  (span-set-property span 'insert-in-front-hooks nil))

(defvar span--priority-maximum 100)

(defun span-set-priority (span pr)
  "Set priority for SPAN. To work around an apparent Emacs bug,
we make priorities negative. Otherwise, region overlays become 
hidden when they overlap our spans. For such negative numbers, a lesser 
number has lower priority. NB: some versions of Emacs may not have
this bug, in which case we can make this function work in a version-dependent 
way."
  (if (< pr 0)
      (error (format "Span priority %s is negative" pr))
    (if (> pr span--priority-maximum)
	(error (format "Span priority %s is greater than the maximum = %s"
		       pr span--priority-maximum))
      (span-set-property span 'priority (1- (- pr span--priority-maximum))))))

(defun span-write-warning (span fun)
  "Give a warning message when SPAN is changed, unless `inhibit-read-only' is non-nil."
  (lexical-let ((fun fun))
    (let ((funs (list (lambda (span afterp beg end &rest args)
			(when (and (not afterp) (not inhibit-read-only))
			    (funcall fun beg end))))))
      (span-set-property span 'modification-hooks funs)
      (span-set-property span 'insert-in-front-hooks funs))))

(defun span-write-warning-subject-to-predicate (span fun pred)
  "Give a warning message when SPAN is changed and the predicate holds, 
unless `inhibit-read-only' is non-nil."
  (lexical-let ((fun fun))
    (let ((funs (list (lambda (span afterp beg end &rest args)
			(when (and (not afterp)
				 (not inhibit-read-only)
				 (funcall pred beg end))
			    (funcall fun beg end))))))
      (span-set-property span 'modification-hooks funs)
      (span-set-property span 'insert-in-front-hooks funs))))

;; We use end first because proof-locked-queue is often changed, and
;; its starting point is always 1
(defun span-lt (s u)
  (or (< (span-end s) (span-end u))
      (and (eq (span-end s) (span-end u))
	   (< (span-start s) (span-start u)))))

(defun spans-filter (overlays prop &optional val)
  "Filter OVERLAYS to those with PROP (optionally matching VAL)."
  (let (ols)
    (dolist (ol overlays)
      (when (span-p ol)
        (let* ((propval (overlay-get ol prop))
               (keep (if val (eq propval val) propval)))
          (when keep
            (push ol ols)))))
    ols))

(defun spans-all ()
  (overlays-in (point-min) (point-max)))

(defun spans-at-point-prop (pt prop)
  (spans-filter (overlays-at pt) prop))

(defun spans-at-region-prop (start end prop)
  "Return a list of the spans in START END with PROP."
  (spans-filter (overlays-in start end) prop))

(defun span-at (pt prop)
  "Return some SPAN at point PT with property PROP."
  (car-safe (spans-at-point-prop pt prop)))

(defun span-at-with-type (pt type)
  "Return some SPAN at point PT with its type property set to TYPE."
  (let* ((all-typed-spans (spans-at-point-prop pt 'type))
	 (type-typed-spans
	  (cl-remove-if-not
	   (lambda (sp) (eq (span-property sp 'type) type))
	   all-typed-spans)))
    (car-safe type-typed-spans)))

(defun span-delete (span)
  "Run the 'span-delete-actions and delete SPAN."
  (mapc (lambda (predelfn) (funcall predelfn))
	(span-property span 'span-delete-actions))
  (delete-overlay span))

(defun span-mark-delete (span)
  "Mark span for potential deletion."
  (span-set-property span 'marked-for-deletion t))

(defun span-unmark-delete (span)
  "Unmark span for deletion."
  (span-set-property span 'marked-for-deletion nil)) ; TODO is this the right way?

(defun span-add-delete-action (span action)
  "Add ACTION to the list of functions called when SPAN is deleted."
  (span-set-property span 'span-delete-actions
		     (cons action (span-property span 'span-delete-actions))))

;; The next two change ordering of list of spans:
(defun span-mapcar-spans (fn start end prop &optional protected-spans)
  "Map function FN over spans between START and END with property PROP."
  (let* ((spans (spans-at-region-prop start end prop))
	 (filtered-spans (if protected-spans
			     (cl-remove-if (lambda (sp) (member sp protected-spans)) spans)
			   spans)))
  (mapcar fn filtered-spans)))

(defun span-mapc-spans (fn start end prop &optional protected-spans)
  "Apply function FN to spans between START and END with property PROP."
  (let* ((spans (spans-at-region-prop start end prop))
	 (filtered-spans (if protected-spans
			     (cl-remove-if (lambda (sp) (member sp protected-spans)) spans)
			   spans)))
  (mapc fn filtered-spans)))

(defun span-mapcar-spans-inorder (fn start end prop)
  "Map function FN over spans between START and END with property PROP."
  (mapcar fn 
	  (sort (spans-at-region-prop start end prop)
		'span-lt)))

;; set span properties from property list
(defun span-set-properties (span props)
  (when (and (consp props)
	     (consp (cdr props)))
    (span-set-property span
		       (car props) (cadr props))
    (span-set-properties span (cddr props))))

(defun span-at-before (pt prop)
  "Return the smallest SPAN at before PT with property PROP.
A span is before PT if it begins before the character before PT."
  (let ((ols (if (eq (point-min) pt)
                 nil ;; (overlays-at pt)
               (overlays-in (1- pt) pt))))
    (setq ols (spans-filter ols prop))
    ;; Eliminate the case of an empty overlay at (1- pt).
    (dolist (ol (prog1 ols (setq ols nil)))
      (if (>= (overlay-end ol) pt) (push ol ols)))
    ;; "Get the smallest".  I have no idea what that means, so I just do
    ;; something somewhat random but vaguely meaningful.  -Stef
    (car (last (sort ols 'span-lt)))))

(defun prev-span (span prop)
  "Return span before SPAN with property PROP."
  (span-at-before (span-start span) prop))

; overlays are [start, end)

(defun next-span (span prop)
  "Return span after SPAN with property PROP."
  ;; Presuming the span-extents.el is the reference, its code does the
  ;; same as the code below.
  (span-at (span-end span) prop))

(defun span-live-p (span)
  "Return non-nil if SPAN is in a live buffer."
  (and span
       (overlay-buffer span)
       (buffer-live-p (overlay-buffer span))))

(defun span-raise (span)
  "Set priority of SPAN to make it appear above other spans."
  ;; FIXME: Emacs already uses a "shorter goes above" which takes care of
  ;; preventing a span from seeing another.  So don't play with
  ;; priorities, please!
  ;; (span-set-priority span 100)
  )

(defun span-string (span)
  (with-current-buffer (overlay-buffer span)
    (buffer-substring-no-properties 
     (overlay-start span) (overlay-end span))))

(defun set-span-properties (span plist)
  "Set SPAN's properties from PLIST which is a plist."
  (while plist
    (overlay-put span (car plist) (cadr plist))
    (setq plist (cddr plist))))

(defun span-at-event (event &optional prop)
  "Find a span at position of EVENT, with property PROP (default 'span)."
  (car (spans-filter
        (overlays-at (posn-point (event-start event)))
        (or prop 'span))))

(defun fold-spans (f &optional buffer from to maparg ignored-flags prop val)
  (with-current-buffer (or buffer (current-buffer))
    (let ((ols (overlays-in (or from (point-min)) (or to (point-max))))
          res)
      ;; Check the PROP.
      (setq ols (spans-filter ols prop val))
      ;; Iterate in order.
      (setq ols (sort ols 'span-lt))
      (while (and ols (not (setq res (funcall f (pop ols) maparg)))))
      res)))

(defun span-detached-p (span)
  "Is this SPAN detached? nil for no, t for yes."
  (null (overlay-buffer span)))

(defun set-span-face (span face)
  "Set the FACE of a SPAN."
  (overlay-put span 'face face))

(defun set-span-keymap (span map)
  "Set the keymap of SPAN to MAP."
  (overlay-put span 'keymap map))

;;
;; Generic functions built on low-level concrete ones.
;;

(defun span-delete-spans (start end prop &optional protected-spans)
  "Delete all spans between START and END with property PROP set."
  (span-mapc-spans 'span-delete start end prop protected-spans))

(defun span-mark-delete-spans (start end prop &optional protected-spans)
  "Mark all spans between START and END with property PROP set for deletion."
  (span-mapc-spans 'span-mark-delete start end prop protected-spans))

(defun span-property-safe (span name)
  "Like span-property, but return nil if SPAN is nil."
  (and span (span-property span name)))

(defun span-set-start (span value)
  "Set the start point of SPAN to VALUE."
  (span-set-endpoints span value (span-end span)))

(defun span-set-end (span value)
  "Set the end point of SPAN to VALUE."
  (span-set-endpoints span (span-start span) value))

;;
;; Handy overlay utils
;;

(defvar span-self-removing-timeout 4)

(defun span-make-self-removing-span (beg end &rest props)
  "Add a self-removing span from BEG to END with properties PROPS.
The span will remove itself after a timeout of 
`span-self-removing-timeout' seconds."
  (let ((ol (span-make beg end)))
    (while props
      (overlay-put ol (car props) (cadr props))
      (setq props (cddr props)))
    (add-timeout span-self-removing-timeout 'delete-overlay ol)
    ol))

(defun span-delete-self-modification-hook (span &rest args)
  "Hook for overlay modification-hooks, which deletes SPAN."
  (if (span-live-p span)
      (span-delete span)))

(defun span-make-modifying-removing-span (beg end &rest props)
  "Add a self-removing span from BEG to END with properties PROPS.
The span will remove itself after any edit within its range.
Return the span."
  (let ((ol (span-make beg end)))
    (while props
      (overlay-put ol (car props) (cadr props))
      (setq props (cddr props)))
    (span-set-property ol 'modification-hooks
                       (list 'span-delete-self-modification-hook))
    ol))



(provide 'span)

;;; span.el ends here
