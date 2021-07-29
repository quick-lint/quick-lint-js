;;; eglot-quicklintjs.el --- Eglot support for quick-lint-js   -*- lexical-binding: t; -*-

;;; Commentary:

;; Eglot support for quick-lint-js.

;; Example usage in your init.el:
;;
;; (require 'eglot-quicklintjs)
;;
;; (defun my-eglot-quicklintjs-setup ()
;;   "Configure eglot-quicklintjs for better experience."
;;
;;   ;; Remove the time to wait after last change before automatically checking
;;   ;; buffer.  The default is 0.5 (500ms)
;;   (setq-local eglot-send-changes-idle-time 0))
;; (add-hook 'js-mode-hook #'my-eglot-quicklintjs-setup)

;;; Code:

(require 'eglot)
(require 'quicklintjs)

(add-to-list 'eglot-server-programs `(js-mode . ,(quicklintjs-find-program
                                                  "--lsp-server")))
(provide 'eglot-quicklintjs)

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

;;; eglot-quicklintjs.el ends here
