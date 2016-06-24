;;; coq-xml.el -- XML functions for Coq when run in server mode
;;;
;;; XML represented in same way as Emacs does, using grammar shown at
;;;
;;;   https://www.emacswiki.org/emacs/XmlParserExamples
;;;

(require 'xml)
(require 'coq-state-vars)

(defvar edit-id-counter 1)

;; these are the same escapes as in Coq's lib/xml_printer.ml, 
;; function buffer_pcdata
(defvar coq-xml-escape-table
  #s(hash-table size 8
                data 
                ( " " "&nbsp;"
                 "<" "&lt;"
                 ">" "&gt;"
                 "\\" "&apos;"
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

(defun coq-xml-tag (xml)
  (car xml))

;; returns body as list, may be several items
(defun coq-xml-body (xml)
  (cddr xml))

;; when we know there's one item only in body
(defun coq-xml-body1 (xml)
  (car (cddr xml)))

; does this XML have this outermost tag
(defun coq-xml-tagp (xml tag)
  (and (not (null xml))
       (listp xml)
       (eq (car xml) tag)))

;; use these functions for specific tags, so we don't make silly mistakes 

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
  (coq-xml-block "string" '() (list s)))

(defun coq-xml-bool (b)
  "XML block with `bool' tag"
  (coq-xml-block "bool" `((val . ,b))
                 '()))

(defun coq-xml-int (n)
  "XML block with `int' tag"
  (coq-xml-block "int" '() (list (format "%d" n))))

(defun coq-xml-unit ()
  "XML block with `unit' tag"
  (coq-xml-block "unit" '() '()))

;; functions that use the `call' tag

(defun coq-xml-init ()
  (coq-xml-call '((val . Init))
                (coq-xml-option '((val . none)))))

;; XML block for text from source file to Coq
;; side-effect: increments edit-id-counter
(defun coq-xml-add-item (item)
  (let ((add-block 
         (coq-xml-call
          '((val . Add))
          (coq-xml-pair
           '()
           (coq-xml-pair 
            '()
            (coq-xml-string (coq-xml-escape item)) 
            (coq-xml-int (- 0 edit-id-counter)))
           (coq-xml-pair
            '()
            (coq-xml-state_id `((val . ,coq-current-state-id)))
            (coq-xml-bool 'true))
           )
          ))
        )
    (setq edit-id-counter (+ edit-id-counter 1))
    add-block))

(defun coq-xml-edit-at (state-id)
  (coq-xml-call '((val . Edit_at))
    (coq-xml-state_id `((val . ,state-id)))))
		
(defun coq-xml-goal ()
  (coq-xml-call '((val . Goal))
                (coq-xml-unit)))

(defun coq-xml-evars ()
  (coq-xml-call '((val . Evars))
                (coq-xml-unit)))

(defun coq-xml-status ()
  (coq-xml-call '((val . Status))
                (coq-xml-bool 'false)))

(defun coq-xml-setoptions (names val-xml)
  (coq-xml-call
   '((val . SetOptions))
   (coq-xml-list 
    '()
    (coq-xml-pair 
     '()
     (apply 'coq-xml-list '() (mapcar 'coq-xml-string names))
     val-xml))))

;; there are a lot of printing options to set via SetOptions
;; so make it not-so-hard to do
(defun coq-xml-printing-options (opts opt-ty opt-val)
  (let ((names (mapcar (lambda (s) (coq-xml-string (symbol-name s)))
                       (cons 'Printing opts))))
    (coq-xml-pair 
     '()
     (apply 'coq-xml-list 
      '()
      names)
     (coq-xml-option_value 
      `((val . ,opt-ty))
      opt-val))))

'(defun coq-xml-setoptions ()
  (coq-xml-call 
   '((val . SetOptions))
   (coq-xml-list 
    '()
    (coq-xml-printing-options
     '(Width)
     'intvalue
     (coq-xml-option 
       '((val . none))))
    (coq-xml-printing-options
     '(Coercions)
     'boolvalue
     (coq-xml-bool 'false))
    (coq-xml-printing-options
     '(Matching)
     'boolvalue
     (coq-xml-bool 'true))
    (coq-xml-printing-options
     '(Notations)
     'boolvalue
     (coq-xml-bool 'true))
    (coq-xml-printing-options
     '(Existential Instances)
     'boolvalue
     (coq-xml-bool 'false))
    (coq-xml-printing-options
     '(Implicit)
     'boolvalue
     (coq-xml-bool 'false))
    (coq-xml-printing-options
     '(All)
     'boolvalue
     (coq-xml-bool 'false))
    (coq-xml-printing-options
     '(Universes)
     'boolvalue
     (coq-xml-bool 'false))
    )))

(defun coq-xml-string-to-xml (s)
  (with-temp-buffer
    (insert s)
    (car (xml-parse-region (point-min) (point-max)))))

(provide 'coq-xml)

