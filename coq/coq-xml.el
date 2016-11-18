;;; coq-xml.el -- XML functions for Coq when run in server mode
;;;
;;; XML represented in same way as Emacs does, using grammar shown at
;;;
;;;   https://www.emacswiki.org/emacs/XmlParserExamples
;;;

(require 'xml)
(require 'cl-lib)
(require 'coq-state-vars)
(require 'coq-system)

;; these are the same escapes as in Coq's lib/xml_printer.ml, 
;; function buffer_pcdata
(defvar coq-xml-escape-table
  #s(hash-table size 8
                data 
                ( " " "&nbsp;"
                 "<" "&lt;"
                 ">" "&gt;"
                 "\'" "&apos;"
                 "\"" "&quot;")
                test equal))

(defun coq-xml-escape (s)
  "Escape string in an XML tree"
  (let ((len (length s))
        (ctr 0)
        (result nil))
    (while (< ctr len)
      (let* ((sch (substring s ctr (+ ctr 1)))
             ; special handling of ampersand
             (esc-str (if (string= sch "&")
                       (if (and (< ctr (- len 1))
                                (string= (substring s (+ ctr 1) (+ ctr 2)) "#"))
                           "&"
                         "&amp;")
                     (gethash sch coq-xml-escape-table sch))))
        (setq result (cons esc-str result)))
      (setq ctr (+ ctr 1)))
    (apply 'concat (reverse result))))

(defun coq-build-xml-attributes (attrs)
  "Build XML attribute string from list of (key . value) pairs"
  (mapconcat 
   (lambda (attr)
     (let* ((key (car attr))
            (val (cdr attr)))
       (format "%s=\"%s\"" key val)))
   attrs
   " "))

;; block within tags
;; contents may be PCData or nested tags
(defun coq-xml-block (tag attrs contents)
  "Create tag pair with attributes"
  "The attrs are (key . value) pairs"
  (let* ((cstr (apply 'concat contents))
         (fmt-attrs (coq-build-xml-attributes attrs))
         (fmt-spaced-attrs (if (string= fmt-attrs "")
                               ""
                             (concat " " fmt-attrs))))
    ; distinguish single-tag and matching-tags cases
    (if (string= cstr "") 
        (concat "<" tag fmt-spaced-attrs "/>")
        (concat "<" tag fmt-spaced-attrs ">"
		cstr
                "</" tag ">\n")))) ;; newline so Coq sees it

(defun coq-xml-attr-value (xml attr-name)
  (let* ((attrs (xml-node-attributes xml))
	 (attr (assq attr-name attrs)))
    (and attr (cdr attr))))

;; this happens a lot
(defun coq-xml-val (xml)
  (coq-xml-attr-value xml 'val))

(defun coq-xml-tag (xml)
  (car xml))

;; returns body as list, may be several items
(defun coq-xml-body (xml)
  (cddr xml))

;; often we know there's only one item in body
(defun coq-xml-body1 (xml)
  (car (cddr xml)))

; does this XML have this outermost tag
(defun coq-xml-tagp (xml tag)
  (and (not (null xml))
       (listp xml)
       (eq (car xml) tag)))

;; use these functions for specific tags, so we don't make mistakes 

;; it would be nice to have a function that took just the tag, and 
;;  returned a function that took the attrs and contents
;; alas, we have dynamic scope here

(defun coq-xml-call (attrs &rest contents)
  "XML block with `call' tag"
  (coq-xml-block "call" attrs contents))

(defun coq-xml-option (attrs &rest contents) 
  "XML block with `option' tag"
  (coq-xml-block "option" attrs contents))

(defun coq-xml-option_value (attrs &rest contents) 
  "XML block with `option_value' tag"
  (coq-xml-block "option_value" attrs contents))

(defun coq-xml-pair (attrs item1 item2) 
  "XML block with `pair' tag"
  (coq-xml-block "pair" attrs `(,item1 ,item2)))

(defun coq-xml-list (attrs &rest contents) 
  "XML block with `list' tag"
  (coq-xml-block "list" attrs contents))

(defun coq-xml-state_id (attrs &rest contents) 
  "XML block with `state_id' tag"
  (coq-xml-block "state_id" attrs contents))

(defun coq-xml-string (s) 
  "XML block with `string' tag"
  (coq-xml-block "string" nil (list s)))

(defun coq-xml-bool (b)
  "XML block with `bool' tag"
  (coq-xml-block "bool" `((val . ,b))
                 nil))

(defun coq-xml-int (n)
  "XML block with `int' tag"
  (coq-xml-block "int" nil `(,(number-to-string n))))

(defun coq-xml-unit ()
  "XML block with `unit' tag"
  (coq-xml-block "unit" nil nil))

;; convenience functions so we don't have to write out traversals by hand

(defun coq-xml-footprint (xml)
  "Footprint to check for a syntactic pattern in parsed XML, 
actually an S-expression. The footprint describes the 
structure of tags only."
  (let ((tag (coq-xml-tag xml)))
    (cons tag 
	  (if (eq tag 'string) 
	      ;; special case for string
	      ;; children are text, so ignore
	      nil
	    (let ((children (coq-xml-body xml)))
	      (cl-remove-if 'null 
			    (mapcar (lambda (child) 
				      (and (consp child)
					   (coq-xml-footprint child)))
				    children)))))))

;; conventional zip using cons, except that
;; path may end, leaving extra xmls, which is OK
(defun zip (xmls paths)
  (let ((null1 (null xmls))
	(null2 (null paths)))
    (if null2
	nil
      (if null1
	  nil ; path too long
	(cons (cons (car xmls) (car paths))
	      (zip (cdr xmls) (cdr paths)))))))

(defun coq-xml-at-path (xml path)
  "Get item parsed XML following PATH, which may terminate in a 
tag, or a tag with an attribute name. Using this function avoids having 
to write out the traversal code by hand each time."
  (if (and (consp path) (or (eq (car path) (coq-xml-tag xml))
			    (eq (car path) '_))) ; wildcard tag
    (cond 
     ;; attribute
     ;; nil is a symbol in this crazy world
     ((and (symbolp (cadr path)) (not (null (cadr path)))) 
      (coq-xml-attr-value xml (cadr path)))
     ;; this XML node
     ((null (cdr path)) 
      xml)
     ;; child nodes, want last one
     (t (let* ((xml-children (coq-xml-body xml))
	       (path-children (cdr path))
	       (zipped-children (zip xml-children path-children))
	       ;; running all of these checks validity of path
	       (results (mapcar (lambda (consed) (coq-xml-at-path (car consed) (cdr consed)))
				zipped-children)))
	  (let (failed)
	    ;; if any item is nil, the path is invalid
	    (dolist (res results failed)
	      (when (null res)
		(setq failed t)))
	    (if failed
		nil
	      (car (reverse results)))))))
    ;; return nil if end of path or tag mismatch
    nil))

;; functions that use the `call' tag

;; XML block for text from source file to Coq
;; side-effect: increments coq-edit-id-counter
(defun coq-xml-add-item (item)
  (let ((add-block 
         (coq-xml-call
          '((val . Add))
          (coq-xml-pair
           nil
           (coq-xml-pair 
            nil
            (coq-xml-string (coq-xml-escape item)) 
            (coq-xml-int (- 0 coq-edit-id-counter)))
           (coq-xml-pair
            nil
            (coq-xml-state_id `((val . ,coq-current-state-id)))
            (coq-xml-bool 'true))
           )
          ))
        )
    (cl-incf coq-edit-id-counter)
    add-block))

(defun coq-xml-query-item (item)
  (coq-xml-call
   '((val . Query))
   (coq-xml-pair
    nil
    (coq-xml-string
     item)
    (coq-xml-state_id `((val . ,coq-current-state-id))))))

(defun coq-xml-init ()
  (coq-xml-call '((val . Init))
                (coq-xml-option '((val . none)))))

;; state-id is string
(defun coq-xml-edit-at (state-id)
  (coq-xml-call 
   '((val . Edit_at))
   (coq-xml-state_id `((val . ,state-id)))))
		
(defun coq-xml-goal ()
  (coq-xml-call '((val . Goal))
                (coq-xml-unit)))

(defun coq-xml-hints ()
  (coq-xml-call '((val . Hints))
                (coq-xml-unit)))


(defun coq-xml-quit ()
  (coq-xml-call '((val . Quit))
                (coq-xml-unit)))

(defun coq-xml-evars ()
  (coq-xml-call '((val . Evars))
                (coq-xml-unit)))

(defun coq-xml-stop-worker (worker-id)
  (coq-xml-call '((val . StopWorker))
		(coq-xml-string worker-id)))

;; Status requests don't force unless we say otherwise
(defun coq-xml-status (&optional b)
  (let ((force (or b 'false)))
    (coq-xml-call '((val . Status))
		  (coq-xml-bool force))))

(defun coq-xml-status-force ()
  (coq-xml-call '((val . Status))
                (coq-xml-bool 'true)))

(defun coq-xml-get-options ()
  (coq-xml-call
   '((val . GetOptions))
   (coq-xml-unit)))

(defun coq-xml-set-options (names val-xml)
  (coq-xml-call
   '((val . SetOptions))
   (coq-xml-list 
    nil
    (coq-xml-pair 
     nil
     (apply 'coq-xml-list nil (mapcar 'coq-xml-string names))
     val-xml))))

;; there are a lot of printing options to set via SetOptions
;; so make it not-so-hard to do
(defun coq-xml-printing-options (opts opt-ty opt-val)
  (let ((names (mapcar (lambda (s) (coq-xml-string (symbol-name s)))
                       (cons 'Printing opts))))
    (coq-xml-pair 
     nil
     (apply 'coq-xml-list 
      nil
      names)
     (coq-xml-option_value 
      `((val . ,opt-ty))
      opt-val))))

(defun coq-xml-string-to-xml (s)
  (with-temp-buffer
    (insert s)
    (libxml-parse-xml-region (point-min) (point-max))))

;; buffer for gluing coqtop responses into XML
;; leading space makes buffer invisible, for the most part
(defvar coq-xml--response-buffer-name " *coq-responses*")
(defvar coq-xml-response-buffer (get-buffer-create coq-xml--response-buffer-name))

;; buffer for gluing coqtop out-of-band responses into XML
;; this is separate from ordinary response buffer because these
;;  OOB responses may occur while processing ordinary responses
(defvar coq-xml--oob-buffer-name " *coq-oob-responses*")
(defvar coq-xml-oob-buffer (get-buffer-create coq-xml--oob-buffer-name))

;;; functions for XML received from coqtop
;;; these assume that current-buffer is a response buffer
;;; though not the *response* buffer seen by user

(defun coq-xml-append-response (s)
  (goto-char (point-max))
  (insert s))

;; the token that replaces &nbsp;
;; in 8.5, just a space
;; in 8.6+, a string token used to preserve structure within richpp tags
;;  eventually replaced with a space
(defvar coq-xml--space-token nil)

(defvar coq-xml-richpp-space-token "#SPC#")

(defun coq-xml-set-space-token ()
  (let* ((full-version (coq-version t))
	 (version (substring full-version 0 3)))
    (setq coq-xml--space-token
	  (if (equal version "8.5")
	      " "
	    coq-xml-richpp-space-token))))

(defun coq-xml-unescape-string (s &optional token)
  (replace-regexp-in-string "&nbsp;" (or token coq-xml--space-token) s))

;; XML parser does not understand &nbsp;
(defun coq-xml-unescape-buffer ()
  (let ((contents (buffer-string)))
    (erase-buffer)
    (insert (coq-xml-unescape-string contents))
    (goto-char (point-min))))

(defun coq-xml-get-next-xml ()
  (ignore-errors
    (let ((xml (libxml-parse-xml-region (point-min) (point-max))))
      (when xml
	(delete-region (point-min) (point-max)))
      xml)))

(provide 'coq-xml)

