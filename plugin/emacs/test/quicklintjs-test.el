;;; quicklintjs-test.el --- quick-lint-js test suite   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'ert)
(require 'package)

(defconst cache-dir-name (concat
                          (expand-file-name default-directory)
                          ".melpa-cache/"))

(defun quicklintjs-install-deps (deps)
  (mapcar (lambda (pkg) (unless (package-installed-p pkg)
                          (if (> emacs-major-version 24)
                              (package-install pkg t)
                            (package-install pkg)))) deps))

(defun quicklintjs-test-main ()
  (setq package-user-dir cache-dir-name
        package-check-signature nil)
  (add-to-list 'package-archives
               '("MELPA" . "https://stable.melpa.org/packages/"))
  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (quicklintjs-install-deps (if (>= emacs-major-version 26)
                                '(flycheck eglot lsp-mode)
                              '(flycheck)))
  (def-flycheck-tests)
  (def-eglot-tests)
  (def-lsp-tests)
  (def-flymake-tests)
  (ert-run-tests-batch-and-exit))

(defun def-flymake-tests ()
  (require 'flymake-quicklintjs)
  (ert-deftest quicklintjs-flymake-parse-errors-and-warnings ()
    (let ((errors-buf (generate-new-buffer "*errors-buf*"))
          (js-buf (generate-new-buffer "*js-buf*")))
      (with-current-buffer js-buf
        (insert "foobar\n")
        (insert "/*💩*/   foobar  \n")
        (insert "foobar /*💩*/\n")
        (insert "function\n"))
      (with-current-buffer errors-buf
        (insert "<stdin>:4:1: error: missing name in function statement [E061]\n")
        (insert "<stdin>:1:1: warning: use of undeclared variable: foobar [E057]\n")
        (insert "<stdin>:2:12: warning: use of undeclared variable: foobar [E057]\n")
        (insert "<stdin>:3:1: warning: use of undeclared variable: foobar [E057]\n")
        (goto-char (point-min))

        (let ((diags (list
                      (flymake-make-diagnostic js-buf 25 31 :warning
                                               "use of undeclared variable: foobar")
                      (flymake-make-diagnostic js-buf 16 22 :warning
                                               "use of undeclared variable: foobar")
                      (flymake-make-diagnostic js-buf 1 7 :warning
                                               "use of undeclared variable: foobar")
                      (flymake-make-diagnostic js-buf 38 46 :error
                                               "missing name in function statement"))))
          (should (equal (flymake-quicklintjs--make-diagnostics js-buf) diags)))))))

(defun def-eglot-tests ()
  (when (>= emacs-major-version 26)
    (require 'eglot-quicklintjs)
    (ert-deftest quicklintjs-is-in-eglot-servers ()
      (should (member '(js-mode "quick-lint-js" "--lsp")  eglot-server-programs)))))

(defun def-lsp-tests ()
  (when (>= emacs-major-version 26)
    (require 'lsp-quicklintjs)
    (ert-deftest quicklintjs-is-in-lsp-clients ()
      (should  (gethash 'quick-lint-js lsp-clients)))))

(defun def-flycheck-tests ()
  (require 'flycheck)
  (require 'flycheck-ert)
  (require 'flycheck-quicklintjs)

  (ert-deftest quicklintjs-is-in-flycheck-checkers ()
    (should (member 'javascript-quicklintjs flycheck-checkers)))

  (flycheck-ert-def-checker-test
   javascript-quicklintjs javascript error
   (let ((flycheck-checker 'javascript-quicklintjs)
         (inhibit-message t))
     (flycheck-ert-should-syntax-check
      "test/error.js" '(js-mode)
      '(1 1 error "missing name in function statement"
          :id "E061" :checker javascript-quicklintjs)
      '(1 12 error "unclosed code block; expected '}' by end of file"
          :id "E134" :checker javascript-quicklintjs)
      '(2 7 error "unexpected token in variable declaration; expected variable name"
          :id "E114" :checker javascript-quicklintjs))))

  (flycheck-ert-def-checker-test
   javascript-quicklintjs javascript warning
   (let ((flycheck-checker 'javascript-quicklintjs)
         (inhibit-message t))
     (flycheck-ert-should-syntax-check
      "test/warning.js" '(js-mode)
      '(1 1 warning "assignment to undeclared variable"
          :id "E059":checker javascript-quicklintjs)))))

;; quick-lint-js finds bugs in JavaScript programs.
;; Copyright (C) 2020  Matthew "strager" Glazar
;;
;; This file is part of quick-lint-js.
;;
;; quick-lint-js is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; quick-lint-js is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.

;;; quicklintjs-test.el ends here
