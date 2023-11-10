;;; quicklintjs-test.el --- quick-lint-js test suite   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'ert)
(require 'package)

(defconst cache-dir-name (concat
                          (expand-file-name default-directory)
                          ".melpa-cache/"))

(setq quicklintjs-test-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun quicklintjs-install-deps (deps)
  (mapcar (lambda (pkg) (unless (package-installed-p pkg)
                          (if (> emacs-major-version 24)
                              (package-install pkg t)
                            (package-install pkg)))) deps))

(defun quicklintjs-test-main ()
  (setq package-user-dir cache-dir-name
        package-check-signature nil)
  ; NOTE(strager): MELPA (non-stable) only supports Emacs 25 and newer, causing
  ; flycheck to fail to install. We choose MELPA Stable because it (as of
  ; February 2023) supports Emacs version 24.
  (add-to-list 'package-archives
               '("MELPA Stable" . "https://stable.melpa.org/packages/"))
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

  ; Disable warning which causes tests to fail when run non-interactively:
  ; "Disabling backend flymake-proc-legacy-flymake because (error Can’t find a suitable init function)"
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (ert-deftest quicklintjs-flymake-parse-errors-and-warnings ()
    (skip-unless (>= emacs-major-version 26))
    (let ((js-buf (generate-new-buffer "*js-buf*")))
      (with-current-buffer js-buf
        (insert "foobar\n")
        (insert "/*💩*/   foobar  \n")
        (insert "foobar /*💩*/\n")
        (insert "function\n"))
      (goto-char (point-min))
      (let ((diags (list
                    (flymake-make-diagnostic
                     js-buf 38 46 :error
                     "missing name in function statement")
                    (flymake-make-diagnostic
                     js-buf 1 7 :warning
                     "use of undeclared variable: foobar")
                    (flymake-make-diagnostic
                     js-buf 16 22 :warning
                     "use of undeclared variable: foobar")
                    (flymake-make-diagnostic
                     js-buf
                     25 31 :warning
                     "use of undeclared variable: foobar")))
            (errors-in (car (read-from-string
                             "(((38 . 46) 0 \"E0061\" \"missing name in \
function statement\")((1 . 7) 2 \"E0057\" \"use of undeclared variable: \
foobar\")((16 . 22) 2 \"E0057\" \"use of undeclared variable: foobar\")(\
(25 . 31) 2 \"E0057\" \"use of undeclared variable: foobar\"))"))))
        (should (equal (flymake-quicklintjs--make-diagnostics js-buf errors-in)
                       diags)))))

  (ert-deftest quicklintjs-exec-with-warning-returns-ok ()
    (let ((js-buf (generate-new-buffer "*js-buf*"))
          (out-buf (generate-new-buffer "*out-buf*")))
      (with-current-buffer js-buf
        (insert "foobar")
        (should (equal (call-process-region (point-min) (point-max)
                                            flymake-quicklintjs-program nil
                                            out-buf nil "--stdin"
                                            "--output-format=emacs-lisp") 0)))))

  (ert-deftest quicklintjs-exec-with-error-returns-ok ()
    (let ((js-buf (generate-new-buffer "*js-buf*"))
          (out-buf (generate-new-buffer "*out-buf*")))
      (with-current-buffer js-buf
        (insert "function")
        (should (equal (call-process-region
                        (point-min) (point-max)
                        flymake-quicklintjs-program nil
                        out-buf nil "--stdin"
                        "--output-format=emacs-lisp") 0)))))

  (ert-deftest quicklintjs-flymake-check-errors-js ()
    (with-temp-buffer
      (javascript-mode)
      (insert-file-contents-literally (expand-file-name "error.js" quicklintjs-test-dir) nil nil nil t)
      (flymake-mode 1)
      (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t)
      (flymake-start)

      (with-timeout (5
                     (ert-fail "Test timed out waiting for diagnostics."))
        ;; TODO(strager): Assert specific diagnostics
        (while (not (flymake-diagnostics))
          (accept-process-output nil 0.01)))))

  ;; This is a regression test. Buffers were mixed up causing
  ;; diagnostics after a certain point (usually a few bytes in) to not
  ;; be cleared.
  (ert-deftest quicklintjs-flymake-fixing-error-clears-diagnostics ()
    (with-temp-buffer
      (javascript-mode)
      (insert "/*xxx*/ consol")
      (flymake-mode 1)
      (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t)
      (flymake-start)

      (with-timeout (5
                     (ert-fail "Test timed out waiting for diagnostics."))
        (while (not (flymake-diagnostics))
          (accept-process-output nil 0.01)))

      (insert "e")  ;; Buffer content: /*xxx*/ console
      (flymake-start)

      (with-timeout (5
                     (ert-fail "Test timed out waiting for diagnostics to be removed."))
        (while (flymake-diagnostics)
          (accept-process-output nil 0.01))))))

(defun def-eglot-tests ()
  (ert-deftest quicklintjs-is-in-eglot-servers ()
    (skip-unless (>= emacs-major-version 26))
    (require 'eglot-quicklintjs)
    (should (member '(js-mode "quick-lint-js" "--lsp-server")
                    eglot-server-programs))))

(defun def-lsp-tests ()
  (ert-deftest quicklintjs-is-in-lsp-clients ()
    (skip-unless (>= emacs-major-version 26))
    (require 'lsp-quicklintjs)
    (should (gethash 'quick-lint-js lsp-clients))))

(defun def-flycheck-tests ()
  (require 'flycheck)
  (require 'flycheck-ert)
  (require 'flycheck-quicklintjs)

  (ert-deftest quicklintjs-is-in-flycheck-checkers ()
    (should (member 'javascript-quicklintjs flycheck-checkers)))

  (flycheck-ert-def-checker-test
   javascript-quicklintjs javascript error-file-checks
   (let ((flycheck-checker 'javascript-quicklintjs)
         (inhibit-message 't))
     (flycheck-ert-should-syntax-check
      "test/error.js" 'js-mode
      '(1 9 error "missing name in function statement"
          :id "E0061" :checker javascript-quicklintjs
          :end-line 1 :end-column 9)
      '(1 12 error "unclosed code block; expected '}' by end of file"
          :id "E0134" :checker javascript-quicklintjs
          :end-line 1 :end-column 13)
      '(2 7 error
          "unexpected token in variable declaration; expected variable name"
          :id "E0114" :checker javascript-quicklintjs
          :end-line 2 :end-column 10))))

  (flycheck-ert-def-checker-test
   javascript-quicklintjs javascript warning-file-checks
   (let ((flycheck-checker 'javascript-quicklintjs)
         (inhibit-message 't))
     (flycheck-ert-should-syntax-check
      "test/warning.js" 'js-mode
      '(1 1 warning "assignment to undeclared variable"
          :id "E0059" :checker javascript-quicklintjs
          :end-line 1 :end-column 2)))))

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
