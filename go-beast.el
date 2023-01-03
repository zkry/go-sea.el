;;; go-beast.el --- IDE Features for Go -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zkry/go-beast
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

(defun go-beast--parent-function (node)
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

;; Lisp object representation of Go types:
;;   Primitive: "int", "uint", "string"
;;   Slice: []int -> (slice* "int")
;;   Struct: Buffer -> "Buffer"
;;   Map:   map[int]string -> (map "int" "string")
;;   Multiple: (int, int , []int) -> (parameters "int" "int" (slice "int"))
;;   Pointer: *int -> (pointer "int")

(defun go-beast--resolve-result (result-node)
  "Return lisp data representing the return types of RESULT-NODE."
  (pcase (treesit-node-type result-node)
    ("type_identifier" (treesit-node-text result-node))
    ("slice_type"
     (let ((element-node
            (alist-get 'elt
                       (treesit-query-capture
                        result-node
                        '((slice_type element: (_) @elt))))))
       (list 'slice (go-beast--resolve-result element-node))))
    ("pointer_type"
     (let ((subtype-node
            (alist-get 'subtype
                       (treesit-query-capture
                        result-node
                        '((pointer_type (_) @subtype :anchor))))))
       (list 'pointer
             (go-beast--resolve-result subtype-node))))
    ("map_type"
     (let* ((map-kv (treesit-query-capture
                     result-node
                     '((map_type key: (_) @key value: (_) @val))))
            (key-node (alist-get 'key map-kv))
            (val-node (alist-get 'val map-kv)))
       (list 'map
             (go-beast--resolve-result key-node)
             (go-beast--resolve-result val-node))))
    ("parameter_list"
     (let* ((types
             (seq-map #'cdr
                      (treesit-query-capture
                       result-node
                       '((parameter_declaration type: (_) @type))))))
       (append '(parameters) (seq-map #'go-beast--resolve-result types))))
    (_ 'unknown*)))

(defconst go-beast--numeric-types
  '("uint8" "uint16" "uint32" "uint64" "int8" "int16" "int32" "int64" "float32" "float64"
    "complex64" "complex128" "byte" "rune" "uint" "int" "uintptr"))

(defun go-beast--zero-value (type &optional err-name)
  "Return the Go zero-value for TYPE."
  (pcase type
    ((pred
      (lambda (type)
        (member type go-beast--numeric-types)))
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
                             (go-beast--zero-value elt err-name))
                           (cdr type))
                  ", "))))

(defconst go-beast-numeric-types
  '(uint8 uint16 uint32 uint64 int8 int16 int32 int64 float32 float64
          complex64 complex128 byte rune uint int uintptr))

(defun go-beast--function-return-types (node)
  "Return lisp data representing the return types of NODE."
  (let* ((func-result
          (alist-get 'result (treesit-query-capture
                              node
                              '((function_declaration result: (_) @result)
                                (method_declaration result: (_) @result))))))
    (go-beast--resolve-result func-result)))

(defun go-beast-insert-error ()
  "Insert an error at point according to the return type."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (func-node (go-beast--parent-function at-node)))
    (if (not func-node)
        (beep)
      (let* ((return (go-beast--function-return-types func-node))
             ;; TODO: extract name of error so it isn't always "err"
             (return-str (go-beast--zero-value return "err"))
             (indent (save-excursion (back-to-indentation) (buffer-substring-no-properties (pos-bol) (point)))))
        (unless (looking-at "\n")
          (end-of-line)
          (insert "\n" indent))
        (insert "if err != nil {\n")
        (insert indent "\t" "return " return-str "\n")
        (insert indent "}")))))

(defun go-beast--to-short-var-declaration (declr-node)
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

(defun go-beast--to-var-declaration (declr-node)
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

(defun go-beast--zero-value-for-type (type-str)
  "Return the string of the zero value for TYPE-STR."
  (ignore-errors
    (with-temp-buffer
      (insert "var x " type-str)
      (go-ts-mode)
      (let* ((root-node (treesit-buffer-root-node))
             (type-node (alist-get 'type (treesit-query-capture root-node '((var_declaration (var_spec type: (_) @type)))))))
        (go-beast--zero-value (go-beast--resolve-result type-node))))))

(defun go-beast--ensure-import (import-path)
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

(defun go-beast-top-level-forms ()
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

(defun go-beast--mod-root ()
  (string-trim-right (shell-command-to-string "go list -m -f '{{.Dir}}'")))

(defun go-beast--mod-pkg-root ()
  (string-trim-right (shell-command-to-string "go list -m -f '{{ .Path }} '")))

;; if from-path = new-path: no references
;; if reference in from-path: add package
;; if reference in new-path: remove package
;; else: remove and add

(defun go-beast-find-references (symbol package from-path new-path)
  "Find all references to SYMBOL of go-mod PACKAGE.
The result is a list of matches in other packages and matches in
the same package."
  (if (equal from-path new-path)
      '(nil nil nil)
    (let* ((original-directory default-directory)
           (default-directory (go-beast--mod-root))
           (files (string-lines (string-trim-right (shell-command-to-string (concat "ag '" symbol "' -l -w")))))
           (matches '())
           (same-to-pkg-matches '())
           (same-from-pkg-matches))
      (dolist (file files)
        (let ((path (concat default-directory "/" file)) ;; TODO: more generic join (windows)
              (from-same-pkg-p (equal (file-name-directory file) from-path))
              (to-same-pkg-p (equal (file-name-directory file) new-path)))
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (point-min))
              (go-ts-mode)
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

(defun go-beast--get-symbol-to-move ()
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

(defun go-beast--move-symbol-top-node (node)
  (cond
   ((equal (treesit-node-type node) "identifier")
    (treesit-node-top-level
     node
     "function_declaration"))
   ((equal (treesit-node-type node) "type_identifier")
    (treesit-node-top-level
     node
     "type_declaration"))))

(defun go-beast--package-id-for-file (file-name)
  "Return the package id for the current file."
  (with-current-buffer (find-file-noselect file-name)
    (go-ts-mode)
    (let* ((root-node (treesit-buffer-root-node))
           (id-node (alist-get 'id
                               (treesit-query-capture
                                root-node
                                '((package_clause (package_identifier) @id))))))
      (when id-node
        (treesit-node-text id-node)))))

(defun go-beast--current-package-id ()
  "Return the package-id of the current file."
  )

(defun go-beast--select-toplevel-form ()
  (let* ((top-level-nodes (go-beast-top-level-forms))
         (names (seq-map #'treesit-node-text top-level-nodes))
         (selection (completing-read-multiple "Select items to move: " names)))
    ;; TODO: is it possible to have a type and function have a same name?
    (seq-filter
     (lambda (node)
       (member (treesit-node-text node) selection))
     top-level-nodes)))

(defun go-beast--move-items (file-name to-move-items)
  "Move the symbol to another package, updating references."
  (let* ((to-move-symbols (seq-map #'treesit-node-text to-move-items))
         (mod-root (go-beast--mod-root))
         (pkg-path (string-trim-left default-directory (concat (regexp-quote mod-root) "/*")))
         (package-name (string-trim-right (file-name-concat (go-beast--mod-pkg-root) pkg-path) "/")))
    (unless (not (equal mod-root ""))
      (user-error "Unable to find go-mod."))
    (unless to-move-items
      (user-error "Nothing movable at current point"))
    (let ((deletions '())
          (insertions '()))
      (dolist (move-item-node to-move-items)
        (let* ((top-level-node (go-beast--move-symbol-top-node move-item-node))
               (top-level-text (treesit-node-text top-level-node))
               (start-marker (make-marker))
               (end-marker (make-marker)))
          (set-marker start-marker (treesit-node-start top-level-node))
          (set-marker end-marker (treesit-node-end top-level-node))
          (push (cons start-marker end-marker) deletions)
          (push (treesit-node-text top-level-node) insertions)))
      (pcase-dolist (`(,start . ,end) deletions)
        (delete-region start end))
      (let* ((new-pkg-id (go-beast--package-id-for-file file-name))
             (new-pkg-directory (file-name-directory file-name))
             (new-pkg-path (string-trim-left new-pkg-directory (concat (regexp-quote mod-root) "/*")))
             (new-pkg-name (string-trim-right (concat (go-beast--mod-pkg-root) "/" new-pkg-path) "/")))
        (dolist (symbol to-move-symbols)
          (let ((symbol-references (go-beast-find-references symbol package-name pkg-path new-pkg-path)))
            (pcase-let ((`(,matches ,from-same-pkg-matches ,to-same-pkg-matches) symbol-references))
              (dolist (match matches)
                (pcase-let ((`(,file . ,pt) match))
                  (find-file (concat mod-root "/" file))
                  (goto-char pt)
                  (let* ((at-node (treesit-node-at (point)))
                         (start (treesit-node-start at-node))
                         (end (treesit-node-end at-node)))
                    (delete-region start (1+ end))
                    (insert new-pkg-id "."))
                  (go-beast--ensure-import new-pkg-name)))
              ;; remove package since the symbol is moving to here
              (dolist (match to-same-pkg-matches)
                (pcase-let ((`(,file . ,pt) match))
                  (find-file (concat mod-root "/" file))
                  (goto-char pt)
                  (let* ((at-node (treesit-node-at (point)))
                         (start (treesit-node-start at-node))
                         (end (treesit-node-end at-node)))
                    (delete-region start (1+ end)))))
              ;; add package since before there was no symbol
              (dolist (match from-same-pkg-matches)
                (pcase-let ((`(,file . ,pt) match))
                  (find-file (concat mod-root "/" file))
                  (goto-char pt)
                  (insert new-pkg-id ".")
                  (go-beast--ensure-import new-pkg-name))))))
        (find-file file-name)
        (go-ts-mode)
        (goto-char (point-max))
        (insert "\n\n")
        (dolist (insertion insertions)
          (insert insertion "\n\n"))))))


;;; Commands:

(defun go-beast-refactor-move (file-name)
  "Move the symbol to another package, updating references."
  (interactive "F")
  (let* ((to-move-items (go-beast--select-toplevel-form)))
    (go-beast--move-items file-name to-move-items)))

(defun go-beast-toggle-var-declaration ()
  "Toggle the type of variable declaration at point."
  (interactive)
  (let*  ((at-node (treesit-node-at (point)))
          (declr-node (treesit-parent-until
                       at-node
                       (lambda (node)
                         (or (equal (treesit-node-type node) "var_declaration")
                             (equal (treesit-node-type node) "short_var_declaration"))))))
    (if (equal (treesit-node-type declr-node) "var_declaration")
        (go-beast--to-short-var-declaration declr-node)
      (go-beast--to-var-declaration declr-node))))

;; TODO: Refactor this to be smaller
(defun go-beast-add-return-type (type)
  "Add a new return type to the function."
  (interactive "sType:")
  (let*  ((zero-value (go-beast--zero-value-for-type type))
          (at-node (treesit-node-at (point)))
          (func-node (go-beast--parent-function at-node))
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
               (func-node (go-beast--parent-function at-node))
               (return-statements
                (treesit-query-capture
                 func-node
                 '((return_statement) @return)))
               (markers '()))
          (pcase-dolist (`(return . ,node) return-statements)
            (when (treesit-node-eq func-node (go-beast--parent-function node))
              (let ((marker (make-marker)))
                (set-marker marker (treesit-node-end node))
                (push marker markers))))
          (dolist (marker markers)
            (goto-char marker)
            (if multiple-args
                (insert ", " zero-value)
              (insert " " zero-value))))))))

(defun go-beast-add-error-return ()
  "Add an error return type to the current function."
  (interactive)
  (go-beast-add-return-type "error"))

(defun go-beast-remove-return-type ()
  "Remove the last return item for the current function."
  (interactive)
  (let*  ((at-node (treesit-node-at (point)))
          (func-node (go-beast--parent-function at-node)))
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
               (func-node (go-beast--parent-function at-node))
               (return-statements
                (treesit-query-capture
                 func-node
                 '((return_statement) @return)))
               (markers '()))
          (pcase-dolist (`(return . ,node) return-statements)
            (when (treesit-node-eq func-node (go-beast--parent-function node))
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

(defun go-beast-flip-if ()
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

(provide 'go-beast)

;;; go-beast.el ends here
