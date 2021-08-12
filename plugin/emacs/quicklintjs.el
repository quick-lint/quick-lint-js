;;; quicklintjs.el --- Helper functions for quicklintjs Emacs plugins   -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Matthew "strager" Glazar

;; Version: 0.0.1
;; Author: Wagner Riffel <w@104d.net>
;; URL: https://quick-lint-js.com
;; Keywords: languages, tools
;; Package-Requires: ((emacs "24.5"))

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

;;; Commentary:

;; Shared parts of configuration and code among others Emacs plugins.

;;; Code:

(defgroup quicklintjs nil
  "quick-lint-js finds bugs in JavaScript programs."
  :prefix "quicklintjs-"
  :group 'tools
  :group 'processes
  :link '(url-link :tag "Website" "https://quick-lint-js.com"))

(defcustom quicklintjs-program-name "quick-lint-js"
  "Quick-lint-js executable name to search."
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
  "Search `quicklintjs-program-name' in variable `exec-path'."
  (executable-find quicklintjs-program-name))

;;;###autoload
(defun quicklintjs-find-program (&rest argv)
  "Make a list of strings by calling `quicklintjs-find-program-function',\
appending `quicklintjs-program-args' and ARGV.
Empty strings and nil are ignored."
  (cl-remove-if-not (lambda (a) (and (stringp a)
                                     (> (length a) 0)))
                    (append (list (funcall
                                   quicklintjs-find-program-function))
                            quicklintjs-program-args argv)))

(provide 'quicklintjs)

;;; quicklintjs.el ends here
