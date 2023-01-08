;;; go-sea.el --- IDE Features for Go -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zkry/go-sea
;; Keywords: 


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; 

;;; Code:

(require 'treesit)

(defgroup go-sea nil
  "Go advanced editing capabilities"
  :prefix "go-sea-"
  :group 'applications)

(defcustom go-sea-symbol-search-pattern
  "ag '%s' -l -w"
  "Command-line program to search entire project for a symbol.
This command should return only the file-names of the results
separated by new-lines."
  :group 'go-sea
  :type 'string)

(defun go-sea--capitalize (str)
  "Capitalize the first letter of STR."
  (concat
   (upcase (substring str 0 1))
   (substring str 1)))

(defun go-sea--parent-function (node)
  "Return the parent of node that is of type `function_declaration'."
  (treesit-parent-until
   node
   (lambda (node)
     (or (equal (treesit-node-type node)
                "function_declaration")
         (equal (treesit-node-type node)
                "method_declaration")
         (equal (treesit-node-type node)
                "func_literal")))))

(defun go-sea--type-to-string (type)
  "Return string of data representaiton of TYPE."
  (pcase type
    (`(slice ,sub-type)
     (concat "[]" (go-sea--type-to-string sub-type)))
    (`(map ,key-type ,val-type)
     (concat "map[" (go-sea--type-to-string key-type) "]"
             (go-sea--type-to-string val-type)))
    (`(pointer-type ,sub-type)
     (concat "*" (go-sea--type-to-string sub-type)))
    (_ type)))

;; Lisp object representation of Go types:
;;   Primitive: "int", "uint", "string"
;;   Slice: []int -> (slice "int")
;;   Struct: Buffer -> "Buffer"
;;   Map:   map[int]string -> (map "int" "string")
;;   Multiple: (int, int , []int) -> (parameters "int" "int" (slice "int"))
;;   Pointer: *int -> (pointer "int")

(defun go-sea--resolve-result (result-node)
  "Return lisp data representing the return types of RESULT-NODE."
  (pcase (treesit-node-type result-node)
    ("type_identifier" (treesit-node-text result-node))
    ("slice_type"
     (let ((element-node
            (alist-get 'elt
                       (treesit-query-capture
                        result-node
                        '((slice_type element: (_) @elt))))))
       (list 'slice (go-sea--resolve-result element-node))))
    ("pointer_type"
     (let ((subtype-node
            (alist-get 'subtype
                       (treesit-query-capture
                        result-node
                        '((pointer-type (_) @subtype :anchor))))))
       (list 'pointer
             (go-sea--resolve-result subtype-node))))
    ("map_type"
     (let* ((map-kv (treesit-query-capture
                     result-node
                     '((map_type key: (_) @key value: (_) @val))))
            (key-node (alist-get 'key map-kv))
            (val-node (alist-get 'val map-kv)))
       (list 'map
             (go-sea--resolve-result key-node)
             (go-sea--resolve-result val-node))))
    ("parameter_list"
     (let* ((types
             (seq-map #'cdr
                      (treesit-query-capture
                       result-node
                       '((parameter_declaration type: (_) @type))))))
       (append '(parameters) (seq-map #'go-sea--resolve-result types))))
    (_ 'unknown*)))

(defun go-sea--resolve-params (parameters-node)
  "Return lisp data representing the parameter types of PARAMETERS-NODE.
Results are in the form of (PARAM-NAMES PARAM-TYPES).  The names
are a list of strings and the types are in the same form as the
return type data."
  (let* ((parameters '())
         (types '())
         (declarations
          (treesit-query-capture
           parameters-node
           '((parameter_declaration) @declr))))
    (pcase-dolist (`(_ . ,declr) declarations)
      (let* ((type
              (alist-get 'type
                         (treesit-query-capture
                          declr
                          '((parameter_declaration
                             (_) @type :anchor)))))
             (resolved-type (go-sea--resolve-result type))
             (identifiers
              (seq-map #'treesit-node-text
               (seq-map #'cdr
                        (treesit-query-capture
                         declr
                         '((identifier) @id))))))
        (setq types (append types (make-list (length identifiers) resolved-type)))
        (setq parameters (append parameters identifiers))))
    (list parameters types)))

(defconst go-sea--numeric-types
  '("uint8" "uint16" "uint32" "uint64" "int8" "int16" "int32" "int64" "float32" "float64"
    "complex64" "complex128" "byte" "rune" "uint" "int" "uintptr"))

(defun go-sea--zero-value (type &optional err-name)
  "Return the Go zero-value for TYPE."
  (pcase type
    ((pred
      (lambda (type)
        (member type go-sea--numeric-types)))
     "0")
    ("error"
     (if err-name
         err-name
       "nil"))
    ("bool"
     "false")
    ("string"
     "\"\"")
    (`(slice ,_)
     "nil")
    (`(map ,_ ,_)
     "nil")
    (`(pointer ,_)
     "nil")
    ((pred
      (lambda (type)
        (and (listp type)
             (eql (car type) 'parameters))))
     (string-join (seq-map (lambda (elt)
                             (go-sea--zero-value elt err-name))
                           (cdr type))
                  ", "))))

(defconst go-sea-numeric-types
  '(uint8 uint16 uint32 uint64 int8 int16 int32 int64 float32 float64
          complex64 complex128 byte rune uint int uintptr))

(defun go-sea--function-return-types (node)
  "Return lisp data representing the return types of NODE."
  (let* ((func-result
          (alist-get 'result (treesit-query-capture
                              node
                              '((function_declaration result: (_) @result)
                                (method_declaration result: (_) @result))))))
    (go-sea--resolve-result func-result)))

(defun go-sea-insert-error ()
  "Insert an error at point according to the return type."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (func-node (go-sea--parent-function at-node)))
    (if (not func-node)
        (beep)
      (let* ((return (go-sea--function-return-types func-node))
             ;; TODO: extract name of error so it isn't always "err"
             (return-str (go-sea--zero-value return "err"))
             (indent (save-excursion (back-to-indentation) (buffer-substring-no-properties (pos-bol) (point)))))
        (unless (looking-at "\n")
          (end-of-line)
          (insert "\n" indent))
        (insert "if err != nil {\n")
        (insert indent "\t" "return " return-str "\n")
        (insert indent "}")))))

(defun go-sea--to-short-var-declaration (declr-node)
  "Convert the var declaration to a short-var declaration."
  (save-excursion
    (let* ((var-data (treesit-query-capture declr-node '((var_declaration "var" @var (var_spec "=" @equal)))))
           (var-node (alist-get 'var var-data))
           (equal-node (alist-get 'equal var-data))
           (var-marker (make-marker))
           (equal-marker (make-marker)))
      (set-marker var-marker (treesit-node-start var-node))
      (set-marker equal-marker (treesit-node-start equal-node))
      (goto-char var-marker)
      (delete-char 3)
      (let ((start (point)))
        (skip-chars-forward " \t")
        (delete-region start (point)))
      (goto-char equal-marker)
      (insert ":")
      (set-marker var-marker nil)
      (set-marker equal-marker nil))))

(defun go-sea--to-var-declaration (declr-node)
  "Convert short-var declaration to var declaration."
  (save-excursion
    (let* ((var-data (treesit-query-capture declr-node '((short_var_declaration ":=" @equal))))
           
           (equal-node (alist-get 'equal var-data))
           (equal-marker (make-marker)))
      (set-marker equal-marker (treesit-node-start equal-node))
      (goto-char (treesit-node-start declr-node))
      (insert "var ")
      (goto-char equal-marker)
      (delete-char 1)
      (set-marker equal-marker nil))))

(defun go-sea--zero-value-for-type (type-str)
  "Return the string of the zero value for TYPE-STR."
  (ignore-errors
    (with-temp-buffer
      (insert "var x " type-str)
      (go-ts-mode)
      (let* ((root-node (treesit-buffer-root-node))
             (type-node (alist-get 'type (treesit-query-capture root-node '((var_declaration (var_spec type: (_) @type)))))))
        (go-sea--zero-value (go-sea--resolve-result type-node))))))

(defun go-sea--ensure-import (import-path)
  "For the current file, ensure that IMPORT-PATH is being imported."
  (save-excursion
    (let* ((root-node (treesit-buffer-root-node))
           (capture (treesit-query-capture
                     root-node
                     `((import_spec
                        ((interpreted_string_literal) @import
                         (:equal @import ,(format "\"%s\"" import-path))))))))
      (unless capture
        (let* ((lparen-node
                (alist-get 'lparen
                           (treesit-query-capture
                            root-node
                            '((import_spec_list ")" @lparen))))))
          (if lparen-node
              (progn
                (goto-char (treesit-node-start lparen-node))
                (insert "\t" (format "\"%s\"" import-path) "\n"))
            (let* ((pkg-node
                    (alist-get
                     'pkg
                     (treesit-query-capture
                      root-node
                      '((package_clause) @pkg)))))
              (goto-char (treesit-node-end pkg-node))
              (insert "\n\n" (format "import \"%s\"" import-path)))))))))

(defun go-sea-top-level-forms ()
  "Return a list of the top level form nodes for the file."
  (let* ((root-node (treesit-buffer-root-node))
         (capture (treesit-query-capture
                   root-node
                   '((source_file
                      (function_declaration
                       name: (identifier) @func))
                     (source_file
                      (type_declaration
                       (type_spec name: (type_identifier) @type))))))
         (result '()))
    (pcase-dolist (`(,_type . ,node) capture)
      (push node result))
    (nreverse result)))


;;; Move file helpers

(defvar go-sea-refactor-context nil
  "Intermediary context for performing project-wide refactoring.")

(defun go-sea-flush-refactor-context ()
  "Write out the pending changes to files."
  (maphash
   (lambda (file-name buffer)
     (with-temp-file file-name
       (insert
        (with-current-buffer buffer
          (buffer-string))))
     (kill-buffer buffer))
   go-sea-refactor-context))

(defmacro go-sea-with-refactor-context (&rest body)
  (declare (debug t))
  `(let ((go-sea-refactor-context (make-hash-table :test 'equal)))
     ,@body
     (go-sea-flush-refactor-context)))

(defun go-sea-get-refactor-buffer-create (file-name)
  (unless go-sea-refactor-context
    (error "no refactoring context"))
  (let ((buf (gethash file-name go-sea-refactor-context)))
    (if buf
        buf
      (let ((new-buf (generate-new-buffer (file-name-nondirectory file-name) )))
        (with-current-buffer new-buf
          (insert-file-contents file-name)
          (treesit-parser-create 'go)
          (goto-char (point-min)))
        (puthash file-name new-buf go-sea-refactor-context)
        new-buf))))

(defmacro go-sea-with-go-ts-file (file-name &rest body)
  "BODY is executed in a buffer with contents of FILE-NAME.
Tree-sitter is enabled for buffer."
  (declare (indent 1) (debug t))
  `(progn
     (let ((buf (go-sea-get-refactor-buffer-create ,file-name)))
       (with-current-buffer buf
         ,@body))))

(defun go-sea--mod-root ()
  (string-trim-right (shell-command-to-string "go list -m -f '{{.Dir}}'")))

(defun go-sea--mod-pkg-root ()
  (string-trim-right (shell-command-to-string "go list -m -f '{{ .Path }} '")))

(defun go-sea-find-references (symbol package from-path new-path)
  "Find all references to SYMBOL of go-mod PACKAGE.
The result is a list of matches in other packages and matches in
the same package."
  (if (equal from-path new-path)
      '(nil nil nil)
    (let* ((original-directory default-directory)
           (default-directory (go-sea--mod-root))
           (files (seq-remove
                   #'string-blank-p
                   (string-lines (string-trim-right (shell-command-to-string (format go-sea-symbol-search-pattern symbol))))))
           (matches '())
           (same-to-pkg-matches '())
           (same-from-pkg-matches))
      (dolist (file files)
        (let ((path (concat default-directory "/" file)) ;; TODO: more generic join (windows)
              (from-same-pkg-p (equal (file-name-directory file) from-path))
              (to-same-pkg-p (equal (file-name-directory file) new-path)))
          (go-sea-with-go-ts-file (file-name-concat default-directory file)
            (save-excursion
              (goto-char (point-min))
              (treesit-parser-create 'go)
              (if from-same-pkg-p
                  ;; TODO: can be in the same directory but be a different testing package.
                  (while (search-forward symbol nil t)
                    (goto-char (match-beginning 0))
                    (let ((at-node (treesit-node-at (point))))
                      (cond
                       ((and (or (equal (treesit-node-type at-node) "type_identifier")
                                 (equal (treesit-node-type at-node) "identifier"))
                             (equal (treesit-node-text at-node) symbol))
                        ;; match!
                        (let ((marker (make-marker)))
                          (set-marker marker (point))
                          (push (cons file marker) same-from-pkg-matches)))))
                    (goto-char (match-end 0)))
                (let* ((pkg-capture
                        (treesit-query-capture
                         (treesit-buffer-root-node)
                         `((import_spec name: (_) :? @name
                                        path: ((interpreted_string_literal) @path
                                               (:equal @path ,(format "\"%s\"" package)))))))
                       (path-ok (alist-get 'path pkg-capture))
                       (pkg-name (or (alist-get 'name pkg-capture) (file-name-base package)))  ;; TODO: not correct
                       (identifier (concat pkg-name "." symbol)))
                  (while (search-forward identifier nil t)
                    (goto-char (match-beginning 0))
                    (save-match-data
                      (let ((parent-node (treesit-node-parent (treesit-node-at (point)))))
                        (cond
                         ((equal (treesit-node-type parent-node) "selector_expression")
                          (let ((expr-capture
                                 (treesit-query-capture
                                  parent-node
                                  '((selector_expression operand: (_) @operand
                                                         field: (_) @field)))))
                            (when (and (equal (treesit-node-text (alist-get 'operand expr-capture)) pkg-name)
                                       (equal (treesit-node-text (alist-get 'field expr-capture)) symbol))
                              (let ((marker (make-marker)))
                                (set-marker marker (point))
                                (if to-same-pkg-p
                                    (push (cons file marker) same-to-pkg-matches)
                                  (push (cons file marker) matches)))
                              ;; match!
                              )))
                         ((equal (treesit-node-type parent-node) "qualified_type")
                          (let ((expr-capture
                                 (treesit-query-capture
                                  parent-node
                                  '((qualified_type package: (_) @package
                                                    name: (_) @name)))))
                            (when (and (equal (treesit-node-text (alist-get 'package expr-capture)) pkg-name)
                                       (equal (treesit-node-text (alist-get 'name expr-capture)) symbol))
                              (let ((marker (make-marker)))
                                (set-marker marker (point))
                                (if to-same-pkg-p
                                    (push (cons file marker) same-to-pkg-matches)
                                  (push (cons file marker) matches))))))
                         (t nil))))
                    (goto-char (match-end 0)))))))))
      (list matches same-from-pkg-matches same-to-pkg-matches))))

(defun go-sea--get-symbol-to-move ()
  "Returns the symbol at point to be moved."
  (let ((at-node (treesit-node-at (point))))
    (cond
     ((equal (treesit-node-type at-node) "identifier")
      (when (equal (treesit-node-type (treesit-node-parent at-node))
                   "function_declaration")
        (cons 'func (treesit-node-text at-node))))
     ((equal (treesit-node-type at-node) "type_identifier")
      (when (equal (treesit-node-type (treesit-node-parent at-node))
                   "type_spec")
        (cons 'type (treesit-node-text at-node)))))))

(defun go-sea--move-symbol-top-node (node)
  (cond
   ((equal (treesit-node-type node) "identifier")
    (treesit-node-top-level
     node
     "function_declaration"))
   ((equal (treesit-node-type node) "type_identifier")
    (treesit-node-top-level
     node
     "type_declaration"))))

(defun go-sea--commenting-node (node)
  "Given a node, return its comment if it exists."
  (save-excursion
    (goto-char (treesit-node-start node))
    (forward-line -1)
    (let ((at-node (treesit-node-at (point))))
      (when (equal (treesit-node-type at-node) "comment")
        at-node))))

(defun go-sea--get-package-id ()
  "Return the packge ID of the current file."
  (let* ((root-node (treesit-buffer-root-node))
         (id-node (alist-get 'id
                             (treesit-query-capture
                              root-node
                              '((package_clause (package_identifier) @id))))))
    (if id-node
        (treesit-node-text id-node)
      (let ((dir-parts (file-name-split (file-name-directory (buffer-file-name)))))
        (nth (- (length dir-parts) 2) dir-parts)))))

(defun go-sea--package-id-for-file (file-name)
  "Return the package id for the current file."
  (go-sea-with-go-ts-file file-name
    (go-ts-mode)
    (go-baest--get-package-id)))

(defun go-sea--select-toplevel-form ()
  (let* ((top-level-nodes (go-sea-top-level-forms))
         (names (seq-map #'treesit-node-text top-level-nodes))
         (selection (completing-read-multiple "Select items to move: " names)))
    ;; TODO: is it possible to have a type and function have a same name?
    (seq-filter
     (lambda (node)
       (member (treesit-node-text node) selection))
     top-level-nodes)))

(defun go-sea--move-items (file-name to-move-items)
  "Move the symbol to another package, updating references."
  (go-sea-with-refactor-context
   (let* ((to-move-symbols (seq-map #'treesit-node-text to-move-items))
          (mod-root (go-sea--mod-root))
          (pkg-path (string-trim-left default-directory (concat (regexp-quote mod-root) "/*")))
          (package-name (string-trim-right (file-name-concat (go-sea--mod-pkg-root) pkg-path) "/")))
     (unless (not (equal mod-root ""))
       (user-error "Unable to find go-mod."))
     (unless to-move-items
       (user-error "Nothing movable at current point"))
     (let ((deletions '())
           (insertions '()))
       (dolist (move-item-node to-move-items)
         (let* ((top-level-node (go-sea--move-symbol-top-node move-item-node))
                (comment-node (go-sea--commenting-node top-level-node))
                (top-level-text (if comment-node
                                    (concat (treesit-node-text comment-node)
                                            "\n"
                                            (treesit-node-text top-level-node))
                                  (treesit-node-text top-level-node)))
                (start-marker (make-marker))
                (end-marker (make-marker)))
           (if comment-node
               (set-marker start-marker (treesit-node-start comment-node))
             (set-marker start-marker (treesit-node-start top-level-node)))
           (set-marker start-marker (treesit-node-start top-level-node))
           (set-marker end-marker (treesit-node-end top-level-node))
           (push (cons start-marker end-marker) deletions)
           (push top-level-text insertions)))
       (pcase-dolist (`(,start . ,end) deletions)
         (delete-region start end))
       (save-buffer)
       (let* ((new-pkg-id (go-sea--package-id-for-file file-name))
              (new-pkg-directory (file-name-directory file-name))
              (new-pkg-path (string-trim-left new-pkg-directory (concat (regexp-quote mod-root) "/*")))
              (new-pkg-name (string-trim-right (concat (go-sea--mod-pkg-root) "/" new-pkg-path) "/")))
         (dolist (symbol to-move-symbols)
           (let ((symbol-references (go-sea-find-references symbol package-name pkg-path new-pkg-path)))
             (pcase-let ((`(,matches ,from-same-pkg-matches ,to-same-pkg-matches) symbol-references))
               (dolist (match matches)
                 (pcase-let ((`(,file . ,pt) match))
                   (go-sea-with-go-ts-file (concat mod-root "/" file)
                     (goto-char pt)
                     (let* ((at-node (treesit-node-at (point)))
                            (start (treesit-node-start at-node))
                            (end (treesit-node-end at-node)))
                       (delete-region start (1+ end))
                       (insert new-pkg-id "."))
                     (go-sea--ensure-import new-pkg-name))))
               ;; remove package since the symbol is moving to here
               (dolist (match to-same-pkg-matches)
                 (pcase-let ((`(,file . ,pt) match))
                   (go-sea-with-go-ts-file (concat mod-root "/" file)
                     (goto-char pt)
                     (let* ((at-node (treesit-node-at (point)))
                            (start (treesit-node-start at-node))
                            (end (treesit-node-end at-node)))
                       (delete-region start (1+ end))))))
               ;; add package since before there was no symbol
               (dolist (match from-same-pkg-matches)
                 (pcase-let ((`(,file . ,pt) match))
                   (go-sea-with-go-ts-file (concat mod-root "/" file)
                     (goto-char pt)
                     (insert new-pkg-id ".")
                     (go-sea--ensure-import new-pkg-name)))))))
         (go-sea-with-go-ts-file file-name
           (go-ts-mode)
           (goto-char (point-max))
           (insert "\n\n")
           (dolist (insertion insertions)
             (insert insertion "\n\n"))))))))


;;; Implement interface helpers

(defconst go-sea-standard-lib-interfaces-cache nil)

(defun go-sea-go-env (var)
  "Fetch the value of the go env value of VAR."
  (let* ((data (json-parse-string (shell-command-to-string "go env -json"))))
    (gethash var data)))

(defun go-sea--standard-lib-interfaces ()
  (let* ((src-dir (file-name-concat (go-sea-go-env "GOROOT") "src"))
         (default-directory src-dir)
         (interfaces (go-sea--list-project-interfaces))
         (usable-interfaces
          (seq-remove
           (lambda (item)
             (pcase-let ((`(,file . _) item))
               (or 
                (string-match-p "/testdata/" file)
                (string-match-p "/vendor/" file)
                (string-match-p "/internal/" file)
                (string-match-p "/usr/local/" file)
                (string-match-p "/cmd/" file)
                (string-match-p "_test.go" file))))
           interfaces)))
    (setq go-sea-standard-lib-interfaces-cache usable-interfaces)
    usable-interfaces))

(defun go-sea--list-project-interfaces ()
  "Return a list of all interface names."
  (let* ((res '())
         (mod-root (go-sea--mod-root))
         (pkg-root (go-sea--mod-pkg-root))
         (relative-directory (string-trim-left default-directory
                                               (regexp-quote (concat mod-root "/")))))
    ;; Private
    (with-temp-buffer
      (call-process "ag" nil (current-buffer) t
                    "^type [a-z][a-zA-Z0-9_]*.* interface {$"
                    "--depth" "1")
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at-p "[a-zA-Z0-9_]")
            (insert relative-directory))
          (forward-line 1)))
      (let ((default-directory mod-root))
        (call-process "ag" nil (current-buffer) t
                      "^type [A-Z][a-zA-Z0-9_]*.* interface {$"))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring (pos-bol) (pos-eol)))
               (parts (string-split line ":"))
               (file (file-name-concat pkg-root (nth 0 parts)))
               (interface-line (nth 2 parts)))
          (string-match "type \\([A-Z][a-zA-Z0-9_]*\\(?:\\[.*\\]\\)?\\) interface {" interface-line)
          (let ((interface-symbol (match-string 1 interface-line)))
            (when interface-symbol
              (push (cons file interface-symbol) res))))
        (forward-line 1))
      res)))

(defun go-sea-subtract-directory (full base)
  (string-trim-left full (concat (regexp-quote base) "/*")))

(defun go-sea--fetch-interface-def (file interface)
  "Fetch the definition of INTERFACE located in FILE.
Results are returned in the form:
(NAME PARAMS RESULT)."
  (let* ((mod-root (go-sea--mod-root)))
    (with-temp-buffer
      (insert-file-contents file)
      (treesit-parser-create 'go)
      (let* ((capture
              (treesit-query-capture
               (treesit-buffer-root-node)
               `((type_declaration
                  (type_spec
                   name: ((_) @name
                          (:equal @name ,interface))
                   type:
                   (interface_type) @interface-type)))))
             (interface-type-node (alist-get 'interface-type capture))
             (method-specs (treesit-query-capture
                            interface-type-node
                            `((method_spec) @method-spec)))
             (results '()))
        (dolist (spec method-specs)
          (let ((capture (treesit-query-capture
                          (cdr spec)
                          '((method_spec
                             name: (_) @name
                             parameters: (_) @parameters
                             result: (_) :? @result)))))
            (push (list (treesit-node-text (alist-get 'name capture))
                        (treesit-node-text (alist-get 'parameters capture))
                        (treesit-node-text (alist-get 'result capture)))
                  results)))
        results))))

(defun go-sea--insert-interface-defs (receiver interfaces)
  "Insert generated Go code of INTERFACES at point for RECEIVER.
INTERFACES should be in the form as provided from the function
`go-sea--fetch-interface-def'."
  (let* ((receiver-name (treesit-node-text receiver))
         (receiver-letter (downcase (substring receiver-name 0 1))))
    (save-excursion
      (insert "\n\n")
      (pcase-dolist (`(,name ,params ,result) interfaces)
        (insert (format "func (%s %s) %s%s %s {\n\tpanic(\"not implemented\")\n}\n\n"
                        receiver-letter
                        receiver-name
                        name
                        params
                        result)))
      (while (looking-at-p "\n")
        (delete-char 1)))))


;;; Commands:

(defun go-sea-refactor-move (file-name)
  "Move the symbol to another package, updating references."
  (interactive "F")
  (let* ((to-move-items (go-sea--select-toplevel-form)))
    (go-sea--move-items file-name to-move-items)))

(defun go-sea-toggle-var-declaration ()
  "Toggle the type of variable declaration at point."
  (interactive)
  (let*  ((at-node (treesit-node-at (point)))
          (declr-node (treesit-parent-until
                       at-node
                       (lambda (node)
                         (or (equal (treesit-node-type node) "var_declaration")
                             (equal (treesit-node-type node) "short_var_declaration"))))))
    (if (equal (treesit-node-type declr-node) "var_declaration")
        (go-sea--to-short-var-declaration declr-node)
      (go-sea--to-var-declaration declr-node))))

;; TODO: Refactor this to be smaller
(defun go-sea-add-return-type (type)
  "Add a new return type to the function."
  (interactive "sType:")
  (let*  ((zero-value (go-sea--zero-value-for-type type))
          (at-node (treesit-node-at (point)))
          (func-node (go-sea--parent-function at-node))
          (multiple-args nil))
    (unless zero-value
      (cond
       ((eql (aref type 0) ?&)
        (user-error "Invalid type %s, did you mean '%s'?" type (concat "*" (substring type 1))))
       (t (user-error "Invalid type %s" type))))
    (if (not func-node)
        (beep)
      (save-excursion
        ;; Part 1: Add return type to function declaration
        (let* ((result (alist-get 'result (treesit-query-capture func-node '((function_declaration result: (_) @result))))))
          (cond
           ((not result)
            ;; no return type... add one
            (let ((parameters-closing-paren
                   (alist-get 'paren
                              (treesit-query-capture
                               func-node
                               '((function_declaration parameters: (parameter_list ")" @paren))
                                 (method_declaration parameters: (parameter_list ")" @paren))
                                 (func_literal parameters: (parameter_list ")" @paren)))))))
              (goto-char (treesit-node-end parameters-closing-paren))
              (insert " " type)))
           ((equal (treesit-node-type result) "parameter_list")
            ;; There is a list of parameters and we need to add one
            (setq multiple-args t)
            (let ((parameters-closing-paren
                   (alist-get 'paren
                              (treesit-query-capture
                               result
                               '((parameter_list ")" @paren))))))
              (goto-char (treesit-node-start parameters-closing-paren))
              (insert ", " type)))
           (t
            ;; There is a single parameter which we need to change to a list.
            (setq multiple-args t)
            (let* ((start-marker (make-marker))
                   (end-marker (make-marker)))
              (set-marker start-marker (treesit-node-start result))
              (set-marker end-marker (treesit-node-end result))
              (goto-char start-marker)
              (insert "(")
              (goto-char end-marker)
              (insert ", " type ")")))))
        ;; Part 2: Add zero value to each return
        (let* ((at-node (treesit-node-at (point)))
               (func-node (go-sea--parent-function at-node))
               (return-statements
                (treesit-query-capture
                 func-node
                 '((return_statement) @return)))
               (markers '()))
          (pcase-dolist (`(return . ,node) return-statements)
            (when (treesit-node-eq func-node (go-sea--parent-function node))
              (let ((marker (make-marker)))
                (set-marker marker (treesit-node-end node))
                (push marker markers))))
          (dolist (marker markers)
            (goto-char marker)
            (if (and multiple-args (not (looking-back "return" (- (point) 10))))
                (insert ", " zero-value)
              (insert " " zero-value))))))))

(defun go-sea-add-error-return ()
  "Add an error return type to the current function."
  (interactive)
  (go-sea-add-return-type "error"))

(defun go-sea-toggle-error-return ()
  "Add an error return type to the current function."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (top-level (or (treesit-node-top-level at-node "function_declaration")
                        (treesit-node-top-level at-node "method_declaration")))
         (result-node (treesit-node-child-by-field-name top-level "result"))
         (err-capture
          (treesit-query-capture
           result-node
           '(((type_identifier) @tid
              (:equal @tid "error"))))))
    (if err-capture
        (go-sea-remove-return-type)
      (go-sea-add-return-type "error"))))

(defun go-sea-remove-return-type ()
  "Remove the last return item for the current function."
  (interactive)
  (let*  ((at-node (treesit-node-at (point)))
          (func-node (go-sea--parent-function at-node)))
    (if (not func-node)
        (beep)
      (save-excursion
        ;; Part 1: Add return type to function declaration
        (let* ((result (alist-get 'result (treesit-query-capture func-node '((function_declaration result: (_) @result)
                                                                             (method_declaration result: (_) @result)
                                                                             (func_literal result: (_) @result))))))
          (cond
           ((not result)
            ;; no return type, do nothing
            (user-error "no return items to remove"))
           ((equal (treesit-node-type result) "parameter_list")
            ;; There is a list of parameters and we need to remove one
            (let* ((last-parameter-capture
                    (treesit-query-capture
                     result
                     '((parameter_list "," :? @comma :anchor (parameter_declaration) @param :anchor ")"))))
                   (last-param-node (alist-get 'param last-parameter-capture))
                   (comma-node (alist-get 'comma last-parameter-capture))
                   (start)
                   (end))
              (if comma-node
                  (setq start (treesit-node-start comma-node))
                (setq start (treesit-node-start last-param-node)))
              (setq end (treesit-node-end last-param-node))
              (delete-region start end)
              (goto-char (1- start))
              (when (looking-at "()")
                (delete-char 2))))
           (t
            (let* ((start-marker (make-marker))
                   (end-marker (make-marker)))
              (delete-region (treesit-node-start result)
                             (treesit-node-end result))))))
        ;; Part 2: Add zero value to each return
        (let* ((at-node (treesit-node-at (point)))
               (func-node (go-sea--parent-function at-node))
               (return-statements
                (treesit-query-capture
                 func-node
                 '((return_statement) @return)))
               (markers '()))
          (pcase-dolist (`(return . ,node) return-statements)
            (when (treesit-node-eq func-node (go-sea--parent-function node))
              (let ((marker (make-marker)))
                (set-marker marker (treesit-node-start node))
                (push marker markers))))
          (dolist (marker markers)
            (goto-char marker)
            (let* ((at-node (treesit-node-at (point)))
                   (return-node (treesit-parent-until
                                 at-node
                                 (lambda (node) (equal (treesit-node-type node) "return_statement"))))
                   (last-arg (treesit-query-capture
                              return-node
                              '((return_statement (expression_list "," :? @comma :anchor (_) @last :anchor)))))
                   (comma-node (alist-get 'comma last-arg))
                   (return-expr-node (alist-get 'last last-arg))
                   (start)
                   (end (treesit-node-end return-expr-node)))
              (if comma-node
                  (setq start (treesit-node-start comma-node))
                (setq start (treesit-node-start return-expr-node)))
              (delete-region start end))))))))

(defun go-sea-flip-if ()
  "Flip the if and else clauses."
  (interactive)
  (save-excursion
    (let* ((at-node (treesit-node-at (point)))
           (if-node (treesit-parent-until
                     at-node
                     (lambda (node)
                       (and
                        node
                        (equal (treesit-node-type node)
                               "if_statement")
                        (treesit-query-capture
                         node
                         '((if_statement condition: (_) @cond
                             consequence: (block) @consequence
                             alternative: (block) @alt-block)))))))
           (if-capture
            (treesit-query-capture
             if-node
             '((if_statement condition: (_) @cond
                             consequence: (block) @consequence
                             alternative: (block) @alt-block))))
           (cond-node (alist-get 'cond if-capture))
           (consequence-node (alist-get 'consequence if-capture))
           (alternative-node (alist-get 'alt-block if-capture))
           (consequence-text (treesit-node-text consequence-node))
           (alternative-text (treesit-node-text alternative-node))
           (cond-start-marker (make-marker))
           (cond-end-marker (make-marker))
           (consequence-start-marker (make-marker))
           (consequence-end-marker (make-marker))
           (alternative-start-marker (make-marker))
           (alternative-end-marker (make-marker)))
      (set-marker cond-start-marker (treesit-node-start cond-node))
      (set-marker cond-end-marker (treesit-node-end cond-node))
      (set-marker consequence-start-marker (treesit-node-start consequence-node))
      (set-marker consequence-end-marker (treesit-node-end consequence-node))
      (set-marker alternative-start-marker (treesit-node-start alternative-node))
      (set-marker alternative-end-marker (treesit-node-end alternative-node))

      ;; delete the block of the if and else part
      (delete-region consequence-start-marker consequence-end-marker)
      (delete-region alternative-start-marker alternative-end-marker)

      ;; insert the other's text
      (goto-char consequence-start-marker)
      (insert alternative-text)
      (goto-char alternative-start-marker)
      (insert consequence-text)

      ;; negate the condition
      (goto-char cond-start-marker)
      (if (looking-at "!(")
          (progn
            (delete-char 2)
            (goto-char cond-end-marker)
            (delete-backward-char 1))
        (insert "!(")
        (goto-char cond-end-marker)
        (insert ")"))
      
      (dolist (marker (list cond-start-marker cond-end-marker consequence-start-marker
                            consequence-end-marker alternative-start-marker alternative-end-marker))
        (set-marker marker nil)))))

(defun go-sea-add-else ()
  "Add else clause."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (if-node (treesit-parent-until
                   at-node
                   (lambda (node)
                     (equal (treesit-node-type node)
                            "if_statement")))))
    (catch 'break
      (while t
        (let ((alt-node (treesit-node-child-by-field-name if-node "alternative")))
          (when (or (not alt-node)
                    (not (equal (treesit-node-type alt-node) "if_statement")))
            (throw 'break nil))
          (setq if-node alt-node))))
    (let ((consequence-node (treesit-node-child-by-field-name if-node "consequence")))
      (goto-char (treesit-node-end consequence-node))
      (insert " else if ")
      (let ((resume-pt (point))
            (indent (buffer-substring-no-properties
                     (pos-bol)
                     (save-excursion
                       (goto-char (pos-bol))
                       (skip-chars-forward "\t")
                       (point)))))
        (insert " {\n\n" indent "}")
        (goto-char resume-pt)))))

(defun go-sea-implement-interface ()
  "Add stub functions to implement an interface."
  (interactive)
  (let* ((at-declr (treesit-node-top-level
                    (treesit-node-at (point))
                    "type_declaration"))
         (_ (unless at-declr
              (user-error "No Type at point")))
         (receiver-symbol
          (alist-get 'name (treesit-query-capture
                            at-declr
                            '((type_spec name: (_) @name)))))
         ;; TODO - prompt type if no type at point
         (project-interfaces (go-sea--list-project-interfaces))
         (mod-root (go-sea--mod-root))
         (pkg-root (go-sea--mod-pkg-root))
         (std-interfaces (go-sea--standard-lib-interfaces))
         (go-env-root (go-sea-go-env "GOROOT"))
         (completions+files (seq-map
                             (lambda (item)
                               (let ((file (car item))
                                     (symbol (cdr item)))
                                 (cons
                                  (concat (string-trim-right (or (file-name-directory file) "") "/")
                                          ":"
                                          symbol)
                                  (cond
                                   ((string-prefix-p pkg-root file)
                                    (file-name-concat mod-root (go-sea-subtract-directory file pkg-root)))
                                   ((string-prefix-p "std/" file)
                                    (file-name-concat go-env-root
                                                      "src"
                                                      (go-sea-subtract-directory file "std")))
                                   (t file)))))
                             (append project-interfaces std-interfaces)))
         (selection (completing-read
                     "Interface:"
                     completions+files
                     nil t))
         (selected-file (alist-get selection completions+files nil nil 'equal))
         (selected-symbol (nth 1 (string-split selection ":")))
         (interfaces (go-sea--fetch-interface-def selected-file selected-symbol)))
    (goto-char (treesit-node-end at-declr))
    (go-sea--insert-interface-defs receiver-symbol interfaces)))

(defun go-sea-demorgans-law ()
  "Use demorgans law to toggle the operators && and ||. "
  (interactive)
  (let* ((at-node (treesit-node-at (point))))
    (if (not (member (treesit-node-text at-node) '("&&" "||")))
        (beep)
      (let* ((binary-expr-node (treesit-node-parent at-node))
             ;; 1. negate binary expression
             (binary-parent-node (treesit-node-parent binary-expr-node))
             (binary-grandparent-node (treesit-node-parent binary-parent-node))
             (expr-marker (make-marker)))
        (set-marker expr-marker (point))
        (cond
         ((and (equal (treesit-node-type binary-parent-node) "parenthesized_expression")
               (equal (treesit-node-type binary-grandparent-node) "unary_expression")
               (equal (treesit-node-text
                       (treesit-node-child-by-field-name binary-grandparent-node "operator"))
                      "!"))
          ;; binary expression is in parenthesis, and that is being negated
          (let ((excl-node (treesit-node-child-by-field-name binary-grandparent-node "operator")))
            (delete-region (treesit-node-start excl-node)
                           (treesit-node-end excl-node))))
         ((equal (treesit-node-type binary-parent-node) "parenthesized_expression")
          ;; binary expression is in parenthesis
          (goto-char (treesit-node-start binary-parent-node))
          (insert "!"))
         (t
          ;; binary expression is not in parenthesis
          (let ((binary-expr-start-marker (make-marker))
                (binary-expr-end-marker (make-marker)))
            (set-marker binary-expr-start-marker (treesit-node-start binary-expr-node))
            (set-marker binary-expr-end-marker (treesit-node-end binary-expr-node))
            (goto-char binary-expr-start-marker)
            (insert "!(")
            (goto-char binary-expr-end-marker)
            (insert ")"))))
        (dolist (field '("left" "right"))
          (goto-char expr-marker)
          (let* ((at-node (treesit-node-at (point)))
                 (binary-expr-node (treesit-node-parent at-node))
                 (child-expr-node (treesit-node-child-by-field-name binary-expr-node field)))
            (cond
             ((and (equal (treesit-node-type child-expr-node) "unary_expression")
                   (equal (treesit-node-text
                           (treesit-node-child-by-field-name child-expr-node "operator")) "!"))
              ;; remove the negation
              (let ((excl-node (treesit-node-child-by-field-name child-expr-node "operator")))
                (delete-region (treesit-node-start excl-node)
                               (treesit-node-end excl-node))))
             ((equal (treesit-node-type child-expr-node) "true")
              ;; just insert a "!"
              (goto-char (treesit-node-start child-expr-node))
              (delete-region (treesit-node-start child-expr-node)
                             (treesit-node-end child-expr-node))
              (insert "false"))
             ((equal (treesit-node-type child-expr-node) "false")
              ;; just insert a "!"
              (goto-char (treesit-node-start child-expr-node))
              (delete-region (treesit-node-start child-expr-node)
                             (treesit-node-end child-expr-node))
              (insert "true"))
             ((equal (treesit-node-type child-expr-node) "parenthesized_expression")
              ;; binary expression is in parenthesis
              (goto-char (treesit-node-start child-expr-node))
              (insert "!"))
             ((member (treesit-node-type child-expr-node) '("call_expression" "identifier" "selector_expression"))
              ;; just insert a "!"
              (goto-char (treesit-node-start child-expr-node))
              (insert "!"))
             (t
              ;; just insert a "!"
              (let ((expr-start (make-marker))
                    (expr-end (make-marker)))
                (set-marker expr-start (treesit-node-start child-expr-node))
                (set-marker expr-end (treesit-node-end child-expr-node))
                (goto-char expr-start)
                (insert "!(")
                (goto-char expr-end)
                (insert ")"))))))
        (goto-char expr-marker)))))

(defun go-sea-fix-eol-commas ()
  "Add missing end-of-line commas where needed."
  (let* ((root-node (treesit-buffer-root-node))
         (capture (treesit-query-capture
                   root-node
                   '((keyed_element) @elem
                     (literal_element) @lit-elem))))
    (save-excursion
      (let ((insertions '()))
        (pcase-dolist (`(_ . ,elem) capture)
          (goto-char (treesit-node-end elem))
          (when (eolp)
            (let ((marker (make-marker)))
              (set-marker marker (point))
              (push marker insertions))))
        (dolist (insertion insertions)
          (goto-char insertion)
          (when (not (looking-at ","))
            (insert ",")))))))


;;; Go Test Generators

(defun go-sea--test-file-name ()
  "Return the file name of the test fale for the current buffer."
  (let* ((file-name (buffer-file-name))
         (directory (file-name-directory file-name))
         (base (file-name-base file-name))
         (extension (file-name-extension file-name)))
    (unless (equal extension "go")
      (error "Not in Go file"))
    (when (string-suffix-p "_test" base)
      file-name)
    (let* ((test-base (concat base "_test")))
      (file-name-concat
       directory
       (concat test-base ".go")))))

(defun go-sea--get-file-buffer-create (file-name)
  "Return a buffer for the current buffers test file.
This function creates a new file with the correct initialization
if the file doesn't already exist."
  (let* ((ret-buffer (generate-new-buffer "*go-test*")))
    (unless (file-exists-p file-name)
      (let* ((package-id (go-sea--get-package-id)))
        (with-temp-file file-name
          (insert (format "package %s" package-id)))))
    (with-current-buffer ret-buffer
      (insert-file-contents file-name)
      (treesit-parser-create 'go))
    ret-buffer))

(defun go-beats--test-input-type-code (params)
  "Generate the \"type _ struct\" portion of the test from PARAMS."
  (pcase-let ((`(,names ,types) params)) 
    (with-temp-buffer
      (insert (format "\ttype args struct {\n"))
      (seq-mapn
       (lambda (name type)
         (insert (format "\t\t%s %s\n" name (go-sea--type-to-string type))))
       names types)
      (insert "\t}\n")
      (buffer-string))))

(defun go-sea--test-args-code (params)
  "Return PARAMS formatted for table test.
For example, if the function has params a and b, this funcion
returns \"tt.args.a, tt.args.b\"."
  (pcase-let ((`(,names _)  params))
    (string-join
     (seq-map
      (lambda (name)
        (format "tt.args.%s" name))
      names)
     ", ")))

(defun go-sea--generate-function-test (func-node &optional receiver-name receiver-type)
  "Generate a test for a Go function."
  (let* ((test-file-name (go-sea--test-file-name))
         (name (treesit-node-text (treesit-node-child-by-field-name func-node "name")))
         (buf (go-sea--get-test-buffer-create test-file-name))
         (params (treesit-node-child-by-field-name func-node "parameters"))
         (result (treesit-node-child-by-field-name func-node "result"))
         (resolved-params (go-sea--resolve-params params))
         (resolved-result (go-sea--resolve-result result))
         (test-name (concat "Test" (go-sea--capitalize name)))
         (input-type-str (go-beats--test-input-type-code resolved-params)))
    (when (not (listp resolved-result))
      (setq resolved-result (list resolved-result)))
    (when (eql (car resolved-result) 'parameters)
      (setq resolved-result (cdr resolved-result)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n")
      (insert (format "func %s(t *testing.T) {\n" test-name))
      (insert input-type-str)
      (insert "\ttests := []struct {\n")
      (insert "\t\tname string\n")
      (insert "\t\targs args\n")
      (let ((n 1))
        (dolist (result resolved-result)
          (if (= n 1)
              (insert (format "\t\twant %s\n" (go-sea--type-to-string result)))
            (insert (format "\t\twant%d %s\n" n (go-sea--type-to-string result))))
          (cl-incf n)))
      (insert "\t}{\n")
      (insert "\t\t// TODO : Add test cases.\n")
      (insert "\t}\n")
      (insert "\tfor _, tt := range tests {\n")
      (insert "\t\tt.Run(tt.name, func(t *testing.T) {\n")
      (when receiver-name
        (insert
         (format "\t\t\t%s := %s{}\n"
                 receiver-name
                 (string-replace "*" "&" (go-sea--type-to-string receiver-type)))))
      (let ((receiver (if receiver-name (concat receiver-name ".") "")))
        (if (> (length resolved-result) 1)
            (let ((gots (seq-map (lambda (n)
                                   (if (= n 1)
                                       "got"
                                     (format "got%d" n)))
                                 (number-sequence 1 (length resolved-result)))))
              (insert (format "\t\t\t%s = %s%s(%s)\n" (string-join gots ", ") receiver name (go-sea--test-args-code resolved-params)))
              (dolist (got gots)
                (let ((want (string-replace "got" "want" got)))
                  (insert (format "\t\t\tif !reflect.DeepEqual(got, tt.%s) {\n" want))
                  (insert (format "\t\t\t\tt.Errorf(\"%s() %s = %%v, %s %%v\", got, tt.%s)\n" name got want want))
                  (insert "\t\t\t}\n"))))
          (insert
           (format "\t\t\tif got := %s%s(%s); !reflect.DeepEqual(got, tt.want) {\n"
                   receiver name (go-sea--test-args-code resolved-params)))
          (insert (format "\t\t\t\tt.Errorf(\"%s() = %%v, want %%v\", got, tt.want)\n" name))
          (insert "\t\t\t}\n")))
      (insert "\t\t})\n")
      (insert "\t}\n")
      (insert "}\n")
      (write-file test-file-name))))

(defun go-sea--generate-method-test (method-node &optional receiver-name receiver-type)
  "Generate a test for a Go function."
  (let* ((receiver-node (treesit-node-child-by-field-name method-node "receiver"))
         (receiver-params (go-sea--resolve-params receiver-node)))
    (seq-let (names types) receiver-params
      (go-sea--generate-function-test method-node (car names) (car types)))))

(defun go-sea-generate-test ()
  "Generate a test for the current item."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (top-level (or (treesit-node-top-level at-node "function_declaration")
                        (treesit-node-top-level at-node "method_declaration")
                        (treesit-node-top-level at-node "type_declaration"))))
    (pcase (treesit-node-type top-level)
      ("function_declaration" (go-sea--generate-function-test top-level))
      ("method_declaration" (go-sea--generate-method-test top-level)))
    top-level))


;;; Jump Commands

(defun go-sea-jump-to-parameters ()
  "Move the point to the parameters of the current function."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (top-level (or (treesit-node-top-level at-node "function_declaration")
                        (treesit-node-top-level at-node "method_declaration")))
         (params-node (treesit-node-child-by-field-name top-level "parameters")))
    (goto-char (1- (treesit-node-end params-node)))))

(defun go-sea-jump-to-result ()
  "Move the point to the result of the current function."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (top-level (or (treesit-node-top-level at-node "function_declaration")
                        (treesit-node-top-level at-node "method_declaration")))
         (params-node (treesit-node-child-by-field-name top-level "parameters"))
         (result-node (treesit-node-child-by-field-name top-level "result")))
    (cond
     ((and result-node
           (equal (treesit-node-type result-node) "parameter_list"))
      (goto-char (1- (treesit-node-end result-node))))
     (result-node
      (goto-char (treesit-node-end result-node)))
     (t
      (goto-char (1+ (treesit-node-end params-node)))))))

(defun go-sea-jump-to-test ()
  "Move the point the current functiosn test."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (top-level (or (treesit-node-top-level at-node "function_declaration")
                        (treesit-node-top-level at-node "method_declaration")))
         (name (treesit-node-text (treesit-node-child-by-field-name top-level "name")))
         (test-name (concat "Test" (go-sea--capitalize name)))
         (test-file-name (go-sea--test-file-name))
         (file-buf (find-file-noselect test-file-name)))
    (set-buffer file-buf)
    (goto-char (point-min))
    (unless (search-forward test-name nil t)
      (goto-char (point-max))
      (insert (format "\nfunc %s(t *testing.T) {\n\t" test-name))
      (let ((pt (point)))
        (insert "\n}")
        (goto-char pt)))
    (switch-to-buffer file-buf)))

(provide 'go-sea)

;;; go-sea.el ends here
