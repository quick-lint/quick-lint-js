;;; flymake-quicklintjs.el ---  Flymake support for quick-lint-js -*- lexical-binding: t; -*-

;;; Commentary:

;; Flymake support for quick-lint-js.

;; Example usage in your init.el:
;;
;; (require 'flymake-quicklintjs)
;;
;; (defun my-flymake-quicklintjs-setup ()
;;   "Configure flymake-quicklintjs for better experience."
;;
;;   ;; Enable Flymake
;;   (unless (bound-and-true-p flymake-mode)
;;     (flymake-mode))
;;   (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t)
;;
;;   ;; Remove the time to wait after last change before automatically checking
;;   ;; buffer.  The default is 0.5 (500ms)
;;   (setq-local flymake-no-changes-timeout 0))
;; (add-hook 'js-mode-hook #'my-flymake-quicklintjs-setup)

;;; Code:

(require 'flymake)

(defgroup flymake-quicklintjs nil
  "Flymake backend for quick-lint-js"
  :link '(url-link :tag "Website" "https://quick-lint-js.com"))

(defcustom flymake-quicklintjs-program "quick-lint-js"
  "Path to quick-lint-js program to run."
  :group 'flymake-quicklintjs
  :type '(file :must-match t))

(defcustom flymake-quicklintjs-args nil
  "Arguments to quick-lint-js."
  :group 'flymake-quicklintjs
  :type '(repeat string))

(defvar-local flymake-quicklintjs--proc nil
  "Internal variable for `flymake-quicklintjs'")

(defun flymake-quicklintjs--make-diagnostics (src-buf quicklintjs-output-alist)
  "Convert QUICKLINTJS-OUTPUT-ALIST to Flymake diagnostic objects.
Return a list of Flymake diagnostic objects in source buffer SRC-BUF."
  (mapcar (lambda (l)
            (let ((region (nth 0 l))
                  (sev (nth 1 l))
                  (msg (nth 3 l)))
              (flymake-make-diagnostic src-buf (car region) (cdr region)
                                       (if (= sev 0) :error :warning) msg)))
          quicklintjs-output-alist))

;;;###autoload
(defun flymake-quicklintjs (report-fn &rest _args)
  "Flymake backend for quick-lint-js linter.
This backend uses `flymake-quicklintjs-program' (which see) to launch a
quick-lint-js process that is passed the current buffer's contents via stdin.
REPORT-FN is Flymake's callback."
  (when (process-live-p flymake-quicklintjs--proc)
    (kill-process flymake-quicklintjs--proc))
  (let ((src-buf (current-buffer))
        (stdout-buf (generate-new-buffer "*flymake-quicklintjs*"))
        (stderr-buf (generate-new-buffer "*flymake-quicklintjs-stderr*")))
    (setq flymake-quicklintjs--proc
          (make-process
           :name "flymake-quicklintjs"
           :connection-type 'pipe
           :noquery t
           :buffer stdout-buf
           :stderr stderr-buf
           :command `(,flymake-quicklintjs-program
                      ,@(let ((file (buffer-file-name)))
                          (if file
                            `("--stdin-path" ,file)
                            ()))
                      "--stdin" "--output-format=emacs-lisp"
                      ,@flymake-quicklintjs-args)
           :sentinel
           (lambda (p _ev)
             (unwind-protect
                 (when (and (eq 'exit (process-status p))
                            (eq p flymake-quicklintjs--proc))
                   (with-current-buffer stderr-buf
                     (let ((stderr-data (buffer-substring-no-properties
                                         (point-min) (point-max))))
                       (if (not (string-empty-p stderr-data))
                           (flymake-log :warning "%S" stderr-data))))
                   (let ((diags (flymake-quicklintjs--make-diagnostics
                                 src-buf
                                 (car (read-from-string
                                       (with-current-buffer stdout-buf
                                         (buffer-substring-no-properties
                                          (point-min) (point-max))))))))
                     (with-current-buffer src-buf
                       (if (or diags (zerop (process-exit-status p)))
                           (funcall report-fn diags
                                    :region (cons (point-min) (point-max)))
                         (funcall report-fn
                                  :panic :explanation
                                  (buffer-substring
                                   (point-min) (progn (goto-char (point-min))
                                                      (line-end-position))))))))
               (unless (process-live-p p)
                 (kill-buffer stdout-buf)
                 (kill-buffer stderr-buf))))))
    (process-send-region flymake-quicklintjs--proc (point-min) (point-max))
    (process-send-eof flymake-quicklintjs--proc)))

(provide 'flymake-quicklintjs)

;; quick-lint-js finds bugs in JavaScript programs.
;; Copyright (C) 2020  Matthew Glazar
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

;;; flymake-quicklintjs.el ends here
