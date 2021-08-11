;;; quicklintjs-pkg.el --- Build quick-lint-js Emacs plugins packages   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Matthew "strager" Glazar

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

;;; Code:

(defun quicklintjs-batch-make-pkg ()
  (setq package-user-dir nil)
  (cl-assert (= (length command-line-args-left) 1))

  (let ((file (car command-line-args-left)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (let* ((pkg-desc (package-buffer-info))
             (pkg-ver (package-desc-version pkg-desc))
             (pkg-dir (format "%s-%s.%s"
                              (file-name-sans-extension file)
                              (car pkg-ver)
                              (car (cdr pkg-ver)))))
        (add-to-list 'package-directory-list pkg-dir)
        (package-unpack pkg-desc)))))

;;; quicklintjs-pkg.el ends here
