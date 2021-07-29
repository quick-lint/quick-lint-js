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
(require 'quicklintjs)

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

(defun flymake-quicklintjs--report-with-crash (qljs-stdout-buf
                                               qljs-stderr-buf
                                               src-buf report-fn)
  "Similar to `flymake-quicklintjs--report' but tries to recover errors from
quick-lint-js crashes and logs whatever is in QLJS-STDERR-BUF"
  (flymake-log :warning
               "quick-lint-js exited with a deadly signal.\n\
Please consider filing a bug at\
 https://github.com/quick-lint/quick-lint-js/issues/new\n\
qjls-stderr-buf:\n\
%s\n"
               (with-current-buffer qljs-stderr-buf
                 (buffer-substring-no-properties (point-min) (point-max))))
  (with-current-buffer qljs-stdout-buf
    (save-excursion
      ;; a crash without any previous issue, try to fix an empty buffer
      (when (zerop (buffer-size))
        (goto-char (point-min))
        (insert-char ?\())
      ;; the above left a '(' dangling or quick-lint-js crashed and
      ;; emacs_lisp_error_reporter::finish() haven't been called yet
      (goto-char (point-min))
      (when (eq (char-after 1) ?\()
        (goto-char (point-max))
        (insert-char ?\)))))
  (condition-case nil
      (flymake-quicklintjs--report qljs-stdout-buf src-buf report-fn)
    (funcall report-fn '())))

(defun flymake-quicklintjs--report (qljs-stdout-buf src-buf report-fn)
  "Call REPORT-FN to highlight reports in SRC_BUF reported in QLJS-STDOUT-BUF.
QLJS-STDOUT-BUF is entirely read and it's expected to be in
QUICKLINTJS-OUTPUT-ALIST format."
  (with-current-buffer qljs-stdout-buf
    (funcall report-fn
             (flymake-quicklintjs--make-diagnostics
              src-buf
              (car (read-from-string
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))))))

;;;###autoload
(defun flymake-quicklintjs (report-fn &rest _args)
  "Flymake backend for quick-lint-js linter.
This backend uses `quicklintjs-find-program' to find and launch a quick-lint-js
process that is passed the current buffer's contents via stdin.
REPORT-FN is Flymake's callback."
  (when (process-live-p flymake-quicklintjs--proc)
    (kill-process flymake-quicklintjs--proc))
  (let ((src-buf (current-buffer))
        (stdout-buf (generate-new-buffer " *flymake-quicklintjs-stdout*"))
        (stderr-buf (generate-new-buffer " *flymake-quicklintjs-stderr*")))
    (save-restriction
      (widen)
      (setq flymake-quicklintjs--proc
            (make-process
             :name "flymake-quicklintjs"
             :connection-type 'pipe
             :noquery t
             :buffer stdout-buf
             :stderr stderr-buf
             :command (quicklintjs-find-program
                       (let ((file (buffer-file-name)))
                         (when file (concat "--path-for-config-search=" file)))
                       "--stdin" "--output-format=emacs-lisp")
             :sentinel
             (lambda (p _ev)
               (let ((proc-status (process-status p)))
                 (unwind-protect
                     (when (and (or (eq proc-status 'exit)
                                    (eq proc-status 'signal))
                                (with-current-buffer src-buf
                                  (eq p flymake-quicklintjs--proc)))
                       (if (zerop (process-exit-status p))
                           (flymake-quicklintjs--report stdout-buf
                                                        src-buf
                                                        report-fn)
                         (flymake-quicklintjs--report-with-crash
                          stdout-buf stderr-buf src-buf report-fn)))
                   (progn (kill-buffer stdout-buf)
                          (kill-buffer stderr-buf)))))))
      (process-send-region flymake-quicklintjs--proc (point-min) (point-max))
      (process-send-eof flymake-quicklintjs--proc))))

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
