;;; quicklintjs.el --- Helper functions for quicklintjs Emacs plugins   -*- lexical-binding: t; -*-
;;; Commentary:

;; Shared parts of configuration and code among others Emacs plugins.

;;; Code:

(require 'cl-seq)

(defgroup quicklintjs nil
  "quick-lint-js finds bugs in JavaScript programs."
  :prefix "quicklintjs-"
  :group 'tools
  :group 'processes
  :link '(url-link :tag "Website" "https://quick-lint-js.com"))

(defcustom quicklintjs-program-name "quick-lint-js"
  "quick-lint-js executable to use."
  :group 'quicklintjs
  :type 'string
  :safe 'stringp)

(defcustom quicklintjs-program-args nil
  "Arguments to `quicklintjs-program-name'."
  :group 'quicklintjs
  :type '(repeat 'string))

(defcustom quicklintjs-find-program-function #'quicklintjs-executable-find
  "Function to find quick-lint-js."
  :group 'quicklintjs
  :type '(choice (const :tag "Search quick-lint-js in `exec-path'"
                        quicklintjs-executable-find)
                 (const :tag "Search quick-lint-js in node-modules"
                        quicklintjs-node-modules-executable-find)
                 (function :tag "Function to get quick-lint-js path")))

;;;###autoload
(defun quicklintjs-executable-find ()
  "Search `quicklintjs-program-name' in `exec-path'."
  (executable-find quicklintjs-program-name))

;;;###autoload
(defun quicklintjs-find-program (&rest argv)
  "Make a list of strings by calling `quicklintjs-find-program-function',
appending `quicklintjs-program-args' and `argv'.
Empty strings and nil are ignored."
  (cl-remove-if-not (lambda (a) (and (stringp a)
                                     (> (length a) 0)))
                    (append (list (funcall
                                   quicklintjs-find-program-function))
                            quicklintjs-program-args argv)))

(provide 'quicklintjs)

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

;;; quicklintjs.el ends here
