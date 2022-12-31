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
   at-node
   (lambda (node)
     (equal (treesit-node-type node)
            "function_declaration"))))

(defun go-beast-insert-error ()
  "Insert an error at point according to the return type."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (func-node (go-beast--parent-function at-node)))
    (when func-node
      (let* ((func-result (treesit-query-capture
                       func-node
                       '((function_declaration result: (_) @result))))
             (return-param-node (alist-get 'result func-result))
             (param-types (treesit-query-capture
                           return-param-node
                           '((parameter_declaration type: (_) @type)))))
        (seq-map
         (lambda (capture)
           (let ((node (cdr capture)))
             (treesit-node-text node)))
         param-types)))))

(provide 'go-beast)

;;; go-beast.el ends here
