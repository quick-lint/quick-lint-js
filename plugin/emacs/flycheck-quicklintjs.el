;;; flycheck-quicklintjs --- quick-lint-js flycheck support   -*- lexical-binding: t; -*-

;;; Commentary:

;; This flycheck exetension configures flycheck to run quick-lint-js
;; to run as a checker.
;;
;; Example usage in your init.el:
;;
;; (require 'flycheck-quicklintjs)
;;
;; ;; Optionally you can setup Flycheck with the expected
;; ;; quick-lint-js experience:
;; (defun my-flycheck-quicklintjs-setup ()
;;   "Configure flycheck-quicklintjs for better experience."
;;
;;   ;; Enable Flycheck
;;   (unless (bound-and-true-p flycheck-mode)
;;     (flycheck-mode))
;;
;;   ;; Use quick-lint-js by default when in 'js-mode`
;;   (flycheck-select-checker 'javascript-quicklintjs)
;;
;;   ;; Remove any delay after a change in buffer to run checkers.
;;   ;; The default is 0.5 (500ms)
;;   (setq-local flycheck-idle-change-delay 0)
;;
;;   ;; Run quick-lint-js program when the buffer is changed and when 'js-mode` is
;;   ;; loaded
;;   (setq-local flycheck-check-syntax-automatically '(mode-enabled idle-change)))
;; (add-hook 'js-mode-hook #'my-flycheck-quicklintjs-setup)
;;

;;; Code:

(require 'flycheck)

(defgroup flycheck-quicklintjs nil
  "quick-lint-js Flycheck integration."
  :prefix "flycheck-"
  :group 'flycheck
  :group 'quicklintjs
  :link '(url-link :tag "Website" "https://quick-lint-js.com"))

(flycheck-def-args-var flycheck-quicklintjs-args javascript-quicklintjs)
(flycheck-def-executable-var javascript-quicklintjs "quick-lint-js")

(flycheck-define-checker javascript-quicklintjs
  "quick-lint-js finds bugs in JavaScript programs.

https://quick-lint-js.com"
  :command	("quick-lint-js"
			 "--output-format=gnu-like"
			 (eval flycheck-quicklintjs-args)
			 source)
  :standard-input 't
  :error-patterns ((error
                    line-start (file-name) ":" line ":" column ":"
                    (zero-or-more whitespace) "error:" (zero-or-more whitespace)
                    (message) line-end)
                   (warning
                    line-start (file-name) ":" line ":" column ":"
                    (zero-or-more whitespace) "warning:" (zero-or-more whitespace)
                    (message) line-end))
  :modes js-mode)

(add-to-list 'flycheck-checkers 'javascript-quicklintjs t)

(provide 'flycheck-quicklintjs)

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

;;; flycheck-quicklintjs.el ends here
