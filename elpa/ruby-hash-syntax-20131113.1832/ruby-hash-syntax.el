;;; ruby-hash-syntax.el --- Toggle ruby hash syntax between classic and 1.9 styles

;; Copyright (C) 2013  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Version: 20131113.1832
;; X-Original-Version: DEV
;; URL: https://github.com/purcell/ruby-hash-syntax
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adapted from the method used by TextMate, this library provides
;; a command `ruby-hash-syntax-toggle' which attempts to automatically
;; convert the selected region of ruby code between 1.8 and 1.9 hash styles.

;;; Code:

;; Borrowed from https://github.com/textmate/ruby.tmbundle/blob/master/Commands/Convert%20Ruby%20hash%20to%201_9%20syntax.tmCommand

;;;###autoload
(defun ruby-toggle-hash-syntax (beg end)
  "Toggle syntax of ruby hash literal in region from BEG to END between ruby 1.8 and 1.9 styles."
  (interactive "r")
  (save-excursion
    (let ((limit (copy-marker (max beg end))))
      (goto-char (min beg end))
      (cond
       ((ruby-hash-syntax--code-has-pattern "=>" limit)
        (ruby-hash-syntax--replace ":\\([a-zA-Z0-9_]+\\) *=> *" "\\1: " limit))
       ((ruby-hash-syntax--code-has-pattern "\\w+:" limit)
        (ruby-hash-syntax--replace "\\([a-zA-Z0-9_]+\\):\\( *\\(?:\"\\(?:\\\"\\|[^\"]\\)*\"\\|'\\(?:\\'\\|[^']\\)*'\\|[a-zA-Z0-9_]+([^)]*)\\|[^,]+\\)\\)" ":\\1 =>\\2" limit))))))

(defun ruby-hash-syntax--code-has-pattern (pat limit)
  "A version of `search-forward' which skips over string literals.
Argument PAT is the search patter, while LIMIT is the maximum
search extent."
  (let (found)
    (save-excursion
      (while (and (not found) (re-search-forward pat limit t))
        (unless (elt (syntax-ppss) 3)
          ;; If this isn't inside a string...
          (setq found t))))
    found))

(defun ruby-hash-syntax--replace (from to end)
  "Replace FROM with TO up to END."
  (while (re-search-forward from end t)
    (replace-match to nil nil)))


(provide 'ruby-hash-syntax)
;;; ruby-hash-syntax.el ends here