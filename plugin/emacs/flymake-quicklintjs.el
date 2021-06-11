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
  :type 'stringp)

(defcustom flymake-quicklintjs-args nil
  "Arguments to quick-lint-js."
  :group 'flymake-quicklintjs
  :type '(repeat 'string))

(defconst flymake-quicklintjs--regexp (concat
                                       "^<stdin>:\\([0-9]+\\):\\([0-9]+\\): "
                                       "\\(warning\\|error\\): "
                                       "\\(.+\\) \\(\\[E[0-9]+\\]\\)$")
  "Regular expression to match quick-lint-js gnu-like output format.")

(defvar-local flymake-quicklintjs--proc nil
  "Internal variable for `flymake-quicklintjs'")

(defun flymake-quicklintjs--error-region (src-buf line col)
  "Compute SRC-BUF region  (BEG . END) corresponding to LINE and COL."
  (with-current-buffer src-buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (goto-char (min (+ (line-beginning-position) col) (line-end-position)))
      (or (bounds-of-thing-at-point 'word) (cons (point) (point))))))

(defun flymake-quicklintjs--make-diagnostics (src-buf)
  "Parse gnu-like compilation messages in current buffer.
Return a list of Flymake diagnostic objects for the source buffer
SRC-BUF."
  (let (diag-accum)
    (while (not (eobp))
      (when (looking-at flymake-quicklintjs--regexp)
        (let* ((line (string-to-number (match-string 1)))
               (col (string-to-number (match-string 2)))
               (type (match-string 3))
               (msg (match-string 4))
               (type-sym (if (string= "error" type) :error :warning)))
          (let* ((diag-region (flymake-quicklintjs--error-region src-buf line col))
                 (beg (car diag-region))
                 (end (min (buffer-size src-buf) (cdr diag-region))))
            (push (flymake-make-diagnostic src-buf beg end type-sym msg)
                  diag-accum))))
      (forward-line 1))
    diag-accum))

;;;###autoload
(defun flymake-quicklintjs (report-fn &rest _args)
  "Flymake backend for quick-lint-js linter.
This backend uses `flymake-quicklintjs-program' (which see) to launch a
quick-lint-js process that is passed the current buffer's contents via stdin.
REPORT-FN is Flymake's callback."
  (when (process-live-p flymake-quicklintjs--proc)
    (kill-process flymake-quicklintjs--proc))
  (let ((src-buf (current-buffer)))
    (setq flymake-quicklintjs--proc
          (make-process
           :name "flymake-quicklintjs"
           :connection-type 'pipe
           :noquery t
           :buffer (generate-new-buffer " *flymake-quicklintjs*")
           :command `(,flymake-quicklintjs-program
                      "--stdin" "--output-format=gnu-like"
                      ,@flymake-quicklintjs-args)
           :sentinel
           (lambda (p _ev)
             (unwind-protect
                 (when (eq 'exit (process-status p))
                   (when (with-current-buffer src-buf (eq p flymake-quicklintjs--proc))
                     (with-current-buffer (process-buffer p)
                       (goto-char (point-min))
                       (let ((diags
                              (flymake-quicklintjs--make-diagnostics src-buf)))
                         (if (or diags (zerop (process-exit-status p)))
                             (funcall report-fn diags
                                      :region (cons (point-min) (point-max)))
                           (funcall report-fn
                                    :panic :explanation
                                    (buffer-substring
                                     (point-min) (progn (goto-char (point-min))
                                                        (line-end-position)))))))))
               (unless (process-live-p p)
                 (kill-buffer (process-buffer p)))))))
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
