;;; go-beast-test.el ---  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'f)

;;; Scaffolding Machenery

(defun go-scaffold--generate-rec (defn at-dir)
  (if (listp (car defn))
      (dolist (item defn)
        (go-scaffold--generate-rec item at-dir))
    (pcase defn
      (`(:mod ,rest)
       (let ((mod-file (file-name-concat at-dir "go.mod")))
         (f-write "module github.com/zkry/test-repo\n\ngo 1.18" 'utf-8 mod-file)
         (go-scaffold--generate-rec rest at-dir)))
      (`(:dir ,name ,contents)
       (let ((dir-file (file-name-concat at-dir name)))
         (f-mkdir dir-file)
         (go-scaffold--generate-rec contents dir-file)))
      (`(:src ,name ,contents)
       (let ((file-name (file-name-concat at-dir name)))
         (f-write contents 'utf-8 file-name))))))

(defvar go-beast-last-scaffold-id nil)

(defun go-scaffold-generate (defn)
  (let* ((temp-dir (temporary-file-directory))
         (proj-name (format "go%d" (random 10000)))
         (proj-dir (file-name-concat temp-dir proj-name "/")))
    (setq go-beast-last-scaffold-id proj-name)
    (f-mkdir proj-dir)
    (go-scaffold--generate-rec defn proj-dir)
    proj-dir))

(defvar go-beast-test-project-root nil)

(defun go-beast-test-file (file)
  (file-name-concat go-beast-test-project-root file))

(defun go-beast-test-file-has-function (file func-name)
  (with-current-buffer (find-file-noselect (go-beast-test-file file))
    (go-ts-mode)
    (treesit-query-capture
     (treesit-buffer-root-node)
     `((function_declaration name: ((_) @name
                                    (:equal @name ,func-name)))))))

(defun go-beast-test-file-has-call-expression (file pkg func)
  "Return non-nil if FILE calls a function with package PKG named FUNC."
  (with-current-buffer (find-file-noselect (go-beast-test-file file))
    (go-ts-mode) ;; TODO: remove these go-ts-mode calls
    (treesit-query-capture
     (treesit-buffer-root-node)
     `((call_expression
        function:
        (selector_expression
         operand: ((_) @pkg
                   (:equal @pkg ,pkg))
         field: ((_) @func
                 (:equal @func ,func))))))))

(defun go-beast-test-file-has-type (file func-name)
  (with-current-buffer (find-file-noselect (go-beast-test-file file))
    (go-ts-mode)
    (treesit-query-capture
     (treesit-buffer-root-node)
     `((type_declaration
        (type_spec name: ((_) @name
                          (:equal @name ,func-name))))))))

(defun go-beast-test-top-level-node (names)
  "Retrun the top level node with text NAME."
  (seq-filter
   (lambda (node)
     (member (treesit-node-text node) names))
   (go-beast-top-level-forms)))

(defmacro go-beast-with-project-setup (fixture path &rest body)
  `(progn
     (let ((auto-mode-alist (append '(("\\.go\\'" . go-ts-mode)) auto-mode-alist))
           (go-beast-test-project-root (go-scaffold-generate ,fixture)))
       (with-current-buffer (find-file-noselect (file-name-concat go-beast-test-project-root ,path))
         (go-ts-mode)
         ,@body))))


;;; Scaffold Definitions

(defconst go-beast-test-fixture-1
  '(:mod
    ((:dir "pkg"
           (:dir "bar"
                 ((:src "new-bar.go"
                        "package bar")
                  (:src "bar.go"
                        "package bar

type Number struct {
	A int
	B int
}

func AddTwoNumbers(a int, b int) int {
	return 10
}
"))))))
  "Simple go project with a single package.")

(defconst go-beast-test-fixture-2
  '(:mod
    ((:dir "pkg"
           ((:dir "baz"
                  ((:src "baz.go"
                         "package baz

func BazSubtract(a int, b int) int {
	return a - b
}
")))
            (:dir "bar"
                  ((:src "new-bar.go"
                         "package bar")
                   (:src "bar.go"
                         "package bar

type Number struct {
	A int
	B int
}

func AddTwoNumbers(a int, b int) int {
	return 10
}
")))))))
  "Go project structure with two packages containing code.")

(defconst go-beast-test-fixture-3
  '(:mod
    ((:dir "pkg"
           ((:dir "baz"
                  ((:src "baz.go"
                         "package baz

func BazSubtract(a int, b int) int {
	return a - b
}
")))
            (:dir "bar"
                  ((:src "new-bar.go"
                         "package bar

func AddFourNumbers(a, b, c, d int) int {
	return AddTwoNumbers(a, b) + AddTwoNumbers(b, c)
}
")
                   (:src "bar.go"
                         "package bar

type Number struct {
	A int
	B int
}

func AddTwoNumbers(a int, b int) int {
	return 10
}
")))))))
  "Function in same package referring to moved function.")


;;; Test Definitions

(ert-deftest go-beast-refactor-move ()
  ;; Test moving one item to another file in the same package.
  (go-beast-with-project-setup go-beast-test-fixture-1 "pkg/bar/bar.go"
    (go-beast--move-items (go-beast-test-file "pkg/bar/new-bar.go") (go-beast-test-top-level-node '("AddTwoNumbers")))
    (should (go-beast-test-file-has-function "pkg/bar/new-bar.go" "AddTwoNumbers"))
    (should (not (go-beast-test-file-has-function "pkg/bar/bar.go" "AddTwoNumbers"))))
  ;; Move two things to a different file in the same package
  (go-beast-with-project-setup go-beast-test-fixture-1 "pkg/bar/bar.go"
    (go-beast--move-items (go-beast-test-file "pkg/bar/new-bar.go") (go-beast-test-top-level-node '("AddTwoNumbers"
                                                                                                    "Number")))
    (should (go-beast-test-file-has-function "pkg/bar/new-bar.go" "AddTwoNumbers"))
    (should (go-beast-test-file-has-type "pkg/bar/new-bar.go" "Number"))
    (should (not (go-beast-test-file-has-function "pkg/bar/bar.go" "AddTwoNumbers")))
    (should (not (go-beast-test-file-has-type "pkg/bar/bar.go" "Number"))))
  ;; Move to different package
  (go-beast-with-project-setup go-beast-test-fixture-2 "pkg/bar/bar.go"
    (go-beast--move-items (go-beast-test-file "pkg/baz/baz.go") (go-beast-test-top-level-node '("AddTwoNumbers"
                                                                                                    "Number")))
    (should (go-beast-test-file-has-function "pkg/baz/baz.go" "AddTwoNumbers"))
    (should (go-beast-test-file-has-type "pkg/baz/baz.go" "Number"))
    (should (not (go-beast-test-file-has-function "pkg/bar/bar.go" "AddTwoNumbers")))
    (should (not (go-beast-test-file-has-type "pkg/bar/bar.go" "Number"))))
  ;; Same file references now import new package
  (go-beast-with-project-setup go-beast-test-fixture-3 "pkg/bar/bar.go"
    (go-beast--move-items (go-beast-test-file "pkg/baz/baz.go") (go-beast-test-top-level-node '("AddTwoNumbers")))
    (should (go-beast-test-file-has-function "pkg/baz/baz.go" "AddTwoNumbers"))
    (should (go-beast-test-file-has-call-expression "pkg/bar/new-bar.go" "baz" "AddTwoNumbers"))
    (should (not (go-beast-test-file-has-function "pkg/bar/bar.go" "AddTwoNumbers"))))
  ;; Test moving a function to a package where it was already being used
  ;; Test moving a function while its being used in the same package
  )

go-beast-last-scaffold-id

(provide 'go-scaffold)

;;; go-scaffold.el ends here
