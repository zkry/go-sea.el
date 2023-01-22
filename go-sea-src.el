;;; go-sea-src.el --- Source code helpers for go-sea -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero

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

;; Source code helpers for go-sea.  This package is meant to be a
;; refactoring of go-sea, containing genaric Go source querying
;; capabilities to build the IDE features off of.

;;; Code:

;;; General Helpers:

(require 'treesit)

(defun go-sea-src-subtract-directory (full base)
  "Remove file-path BASE from FULL."
  (string-trim-left full (concat (regexp-quote base) "/*")))

(defun go-sea-src-directory-.. (file-name)
  "Return a directory string one above FILE-NAME."
  (let* ((parts (file-name-split file-name))
         (last-part (car (last parts))))
    (if (equal last-part "")
        (string-join (butlast parts 2) "/")
      (string-join (butlast parts) "/"))))

;;; Treesit source helpers

(defun go-sea-src-get-package-id ()
  "Return the packge ID of the current file."
  (let* ((root-node (treesit-buffer-root-node))
         (id-node (alist-get 'id
                             (treesit-query-capture
                              root-node
                              '((package_clause (package_identifier) @id))))))
    (if id-node
        (treesit-node-text id-node t)
      (let ((dir-parts (file-name-split (file-name-directory (buffer-file-name)))))
        (nth (- (length dir-parts) 2) dir-parts)))))

(defun go-sea-src-pkg-id-to-path (pkg-id)
  ""
  (let* ((import-spec-names (treesit-query-capture
                             (treesit-buffer-root-node)
                             `((import_spec
                                name: ((_) @name
                                       (:equal @name ,pkg-id))
                                path: (_) @path)))))
    (if import-spec-names
        (string-trim (treesit-node-text (alist-get 'path import-spec-names) t) "\"" "\"")
      (let* ((import-path-names
              (treesit-query-capture
               (treesit-buffer-root-node)
               '((import_spec path: (_) @path)))))
        (seq-find
                   (lambda (path)
                     (string-match-p
                      (rx string-start
                          (literal pkg-id)
                          (or string-end
                              (literal "_")))
                      (file-name-nondirectory path)))
                   (seq-map (lambda (capture)
                              (string-trim (treesit-node-text (cdr capture) t) "\"" "\""))
                            import-path-names))))))

;;; Indexing Functions
(defun go-sea-search-id-def-in-dir (id dir)
  "Return the definition of ID located in directory DIR."
  (with-temp-buffer
    (let* ((default-directory dir)
           (res (call-process "ag" nil (current-buffer) t
                              (format "^(?:func|type|var) +%s[\\n ]" id)
                              "--depth" "1")))
      (unless (= res 0)
        (error "Identifier `%s' not found" id))
      ;; multiple definitions of the same name are not allowed
      (pcase-let ((`(,file ,line ,_match) (string-split (car (string-lines (string-trim-right (buffer-string)) t)) ":")))
        (list (file-name-concat dir file)
              (string-to-number line))))))


;;; Go Module Helpers:

(defun go-sea-src-mod-dir ()
  "Retrun the directory of the Go module."
  (string-trim-right (shell-command-to-string "go list -m -f '{{.Dir}}'")))

(defun go-sea-src-mod-path ()
  "Retrun the base path of the Go module."
  (string-trim-right (shell-command-to-string "go list -m -f '{{.Path}}'")))

(defun go-sea-src-package-dir (package)
  "Return the file directory of PACKAGE."
  (string-trim-right (shell-command-to-string (format "go list -f '{{.Dir}}' '%s'" package))))

(defun go-sea-src-mod-path-full ()
  "Return the full path of a Go modules."
  (let* ((mod-path (go-sea-src-mod-path))
         (mod-dir (go-sea-src-mod-dir))
         (pkg-id (go-sea-src-get-package-id))
         (package-path (file-name-concat mod-path (go-sea-src-subtract-directory default-directory mod-dir))))
    (file-name-concat (go-sea-src-directory-.. package-path) pkg-id)))

(defun go-sea-src--resolve-type-identifier (node)
  ""
  (go-sea-search-id-def-in-dir (treesit-node-text node t) default-directory))

(defun go-sea-src--resolve-qualified-type (node)
  "Return the location of a qualified type NODE.
Result is in the form (FULL-PATH LINE)."
  (let* ((capture (treesit-query-capture
                   node
                   '((qualified_type package: (_) @package
                                     name: (_) @name))))
         (package-node (alist-get 'package capture))
         (name-node (alist-get 'name capture))
         (full-package (go-sea-src-pkg-id-to-path (treesit-node-text package-node t)))
         (package-directory (go-sea-src-package-dir full-package)))
    (go-sea-search-id-def-in-dir (treesit-node-text name-node t) package-directory)))

(defun go-sea-src-find-definition (node)
  "Return the definition of NODE.
Go definitions can be the following types: type_declaration,
var_declaration, function_declaration, or method_declaration."
  ;; identifier -> function_declaration, var_declaration (in same package)
  ;; selector_expression -> function_declaration, var_declaration, method_declaration (in other package)
  ;; qualified_type -> type declaration (in other package)
  ;; type_identifier -> type declaration (in same package)
  (let* ((parent-node (treesit-node-parent node)))
    (pcase (list (treesit-node-type parent-node) (treesit-node-type node))
      (`("selector_expression" ,_)
       (error "Not implemented"))
      (`("qualified_type" ,_)
       (go-sea-src--resolve-qualified-type parent-node))
      (`(,_ "identifier")
       (error "Not implemented"))
      (`(,_ "type_identifier")
       (go-sea-src--resolve-type-identifier node)))))

(provide 'go-sea-src)

;;; go-sea-src.el ends here
