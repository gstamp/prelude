;;; elixir-mode.el --- Major mode for editing Elixir files

;; Copyright 2011-2015 secondplanet
;;           2013-2015 Samuel Tonini, Matt DeBoard, Andreas Fuchs
;; Authors: Humza Yaqoob,
;;          Andreas Fuchs <asf@boinkor.net>,
;;          Matt DeBoard
;;          Samuel Tonini <tonini.samuel@gmail.com>

;; URL: https://github.com/elixir-lang/emacs-elixir
;; Created: Mon Nov 7 2011
;; Keywords: languages elixir
;; Version: 2.2.6
;; Package-Requires: ((emacs "24") (pkg-info "0.4"))

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Provides font-locking, indentation and navigation support
;;  for the Elixir programming language.

;;; Code:

(require 'easymenu)           ; for menubar features
(require 'elixir-smie)        ; syntax and indentation support

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :prefix "elixir-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/elixir-lang/emacs-elixir")
  :link '(emacs-commentary-link :tag "Commentary" "elixir-mode"))

(defvar elixir-mode-website-url "http://elixir-lang.org"
  "Official url of Elixir programming website.")

(defvar elixir-mode-hook nil)

(defvar elixir-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `elixir-mode'.")

(defvar elixir-imenu-generic-expression
  '(("Modules" "^\\s-*defmodule[ \n\t]+\\([A-Z][A-Za-z0-9._]+\\)\\s-+.*$" 1)
    ("Public Functions" "^\\s-*def[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Private Functions" "^\\s-*defp[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Public Macros" "^\\s-*defmacro[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Private Macros" "^\\s-*defmacrop[ \n\t]+\\([a-z0-9_!\\?]+\\)\\(([^)]*)\\)*.*" 1)
    ("Delegates" "^\\s-*defdelegate[ \n\t]+\\([a-z0-9_]+\\)\\(([^)]*)\\)*.*" 1)
    ("Overridables" "^\\s-*defoverridable[ \n\t]+\\([a-z0-9_]+\\)\\(([^)]*)\\)*.*" 1)
    ("Tests" "^\\s-*test[ \t\n]+\"?\\(:?[a-z0-9_@+() \t-]+\\)\"?[ \t\n]+.*" 1))
  "Imenu pattern for `elixir-mode'.")

(defvar elixir-basic-offset 2)
(defvar elixir-key-label-offset 0)
(defvar elixir-match-label-offset 2)

(defvar elixir-operator-face 'elixir-operator-face)
(defface elixir-operator-face
  '((((class color) (min-colors 88) (background light))
     :foreground "darkred")
    (((class color) (background dark))
     (:foreground "lemonchiffon1"))
    (t nil))
  "For use with operators."
  :group 'font-lock-faces)

(defvar elixir-negation-face 'elixir-negation-face)
(defface elixir-negation-face
  '((((class color) (min-colors 88) (background light))
     :foreground "#ff4500")
    (((class color) (background dark))
     (:foreground "#ff4500"))
    (t nil))
  "For use with standalone \"?\" to indicate code point."
  :group 'font-lock-faces)

(defvar elixir-attribute-face 'elixir-attribute-face)
(defface elixir-attribute-face
  '((((class color) (min-colors 88) (background light))
     :foreground "MediumPurple4")
    (((class color) (background dark))
     (:foreground "thistle"))
    (t nil))
  "For use with module attribute tokens."
  :group 'font-lock-faces)

(defvar elixir-atom-face 'elixir-atom-face)
(defface elixir-atom-face
  '((((class color) (min-colors 88) (background light))
     :foreground "RoyalBlue4")
    (((class color) (background dark))
     (:foreground "light sky blue"))
    (t nil))
  "For use with atoms & map keys."
  :group 'font-lock-faces)

(eval-when-compile
  (defconst elixir-rx-constituents
    `(
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by escaped quote
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or "\"" "\"\"\"" "'" "'''")))))
      (atoms . ,(rx ":"
                    (or
                     (and
                      (any "a-z" "A-Z" "_" "\"" "'")
                      (zero-or-more (any "a-z" "A-Z" "0-9" "_" "\"" "'")))
                     (and "\"" (one-or-more (not (any "\""))) "\"")
                     (and "'" (one-or-more (not (any "'"))) "'"))))
      (builtin . ,(rx symbol-start
                      (or "case" "cond" "for" "if" "unless" "try" "receive"
                          "raise" "quote" "unquote" "unquote_splicing" "throw"
                          "super")
                      symbol-end))
      (builtin-declaration . ,(rx symbol-start
                                  (or "def" "defp" "defmodule" "defprotocol"
                                      "defmacro" "defmacrop" "defdelegate"
                                      "defexception" "defstruct" "defimpl"
                                      "defcallback")
                                  symbol-end))
      (builtin-namespace . ,(rx symbol-start
                                (or "import" "require" "use" "alias")
                                symbol-end))
      ;; Set aside code point syntax for `elixir-negation-face'.
      (code-point . ,(rx symbol-start
                         "?"
                         anything
                         symbol-end))
      (function-declaration . ,(rx symbol-start
                                   (or "def" "defp")
                                   symbol-end))
      ;; Match `@doc' or `@moduledoc' syntax, with or without triple quotes.
      (heredocs . ,(rx symbol-start
                       (or "@doc" "@moduledoc" "~s")
                       symbol-end))
      ;; The first character of an identifier must be a letter or an underscore.
      ;; After that, they may contain any alphanumeric character + underscore.
      ;; Additionally, the final character may be either `?' or `!'.
      (identifiers . ,(rx (one-or-more (any "A-Z" "a-z" "_"))
                          (zero-or-more (any "A-Z" "a-z" "0-9" "_"))
                          (optional (or "?" "!"))))
      (keyword . ,(rx symbol-start
                      (or "fn" "do" "end" "after" "else" "rescue" "catch")
                      symbol-end))
      (keyword-operator . ,(rx symbol-start
                               (or "not" "and" "or" "when" "in")
                               symbol-end))
      ;; Module and submodule names start with upper case letter. This
      ;; can then be followed by any combination of alphanumeric chars.
      ;; In turn, this can be followed by a `.' which begins the notation of
      ;; a submodule, which follows the same naming pattern of the module.
      ;; Finally, like other identifiers, it can be terminated with either `?'
      ;; or `!'.
      (module-names . ,(rx symbol-start
                           (optional (or "%" "&"))
                           (one-or-more (any "A-Z"))
                           (zero-or-more (any "A-Z" "a-z" "_" "0-9"))
                           (zero-or-more
                            (and "."
                                 (one-or-more (any "A-Z" "_"))
                                 (zero-or-more (any "A-Z" "a-z" "_" "0-9"))))
                           (optional (or "!" "?"))
                           symbol-end))
      (pseudo-var . ,(rx symbol-start
                         (optional (or "%" "&"))
                         (or "_" "__MODULE__" "__DIR__" "__ENV__" "__CALLER__"
                             "__block__" "__aliases__")
                         symbol-end))
      (sigils . ,(rx "~" (or "B" "C" "R" "S" "b" "c" "r" "s" "w")))))

  (defmacro elixir-rx (&rest sexps)
    (let ((rx-constituents (append elixir-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))

(defsubst elixir-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun elixir-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (elixir-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))


(defun elixir-syntax-propertize-interpolation ()
  (let* ((beg (match-beginning 0))
         (context (save-excursion (save-match-data (syntax-ppss beg)))))
    (put-text-property beg (1+ beg) 'elixir-interpolation
                       (cons (nth 3 context) (match-data)))))

(defconst elixir-syntax-propertize-function
  (syntax-propertize-rules
   ((elixir-rx string-delimiter)
    (0 (ignore (elixir-syntax-stringify))))
   ((rx (group "#{" (0+ (not (any "}"))) "}"))
    (0 (ignore (elixir-syntax-propertize-interpolation))))))

(defun elixir-syntax-propertize-function (start end)
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ((elixir-rx string-delimiter)
       (0 (ignore (elixir-syntax-stringify))))
      ((rx (group "#{" (0+ (not (any "}"))) "}"))
       (0 (ignore (elixir-syntax-propertize-interpolation)))))
     start end)))

(defun elixir-match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point) 'elixir-interpolation
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'elixir-interpolation)))
        (if (eq (car value) ?\")
            (progn
              (set-match-data (cdr value))
              t)
          (elixir-match-interpolation limit))))))


(defconst elixir-font-lock-keywords
  `(
    ;; String interpolation
    (elixir-match-interpolation 0 font-lock-variable-name-face t)

    ;; Module attributes
    (,(elixir-rx (group (or heredocs
                            (and "@" (1+ identifiers)))))
     1 elixir-attribute-face)

    ;; Keywords
    (,(elixir-rx (group (or builtin builtin-declaration builtin-namespace
                            keyword keyword-operator)))
     1 font-lock-keyword-face)

    ;; Function names, i.e. `def foo do'.
    (,(elixir-rx (group function-declaration)
                 space
                 (group identifiers))
     2 font-lock-function-name-face)

    ;; Sigils
    (,(elixir-rx (group sigils))
     1 font-lock-builtin-face)

    ;; Sigil patterns. Elixir has support for eight different sigil delimiters.
    ;; This isn't a very DRY approach here but it gets the job done.
    (,(elixir-rx sigils
                 (and "/" (group (one-or-more (not (any "/")))) "/"))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "[" (group (one-or-more (not (any "]")))) "]"))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "{" (group (one-or-more (not (any "}")))) "}"))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "(" (group (one-or-more (not (any ")")))) ")"))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "|" (group (one-or-more (not (any "|")))) "|"))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "\"" (group (one-or-more (not (any "\"")))) "\""))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "'" (group (one-or-more (not (any "'")))) "'"))
     1 font-lock-string-face)
    (,(elixir-rx sigils
                 (and "<" (group (one-or-more (not (any ">")))) ">"))
     1 font-lock-string-face)

    ;; Regex patterns. Elixir has support for eight different regex delimiters.
    ;; This isn't a very DRY approach here but it gets the job done.
    (,(elixir-rx "~r"
                 (and "/" (group (one-or-more (not (any "/")))) "/"))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "[" (group (one-or-more (not (any "]")))) "]"))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "{" (group (one-or-more (not (any "}")))) "}"))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "(" (group (one-or-more (not (any ")")))) ")"))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "|" (group (one-or-more (not (any "|")))) "|"))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "\"" (group (one-or-more (not (any "\"")))) "\""))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "'" (group (one-or-more (not (any "'")))) "'"))
     1 font-lock-string-face)
    (,(elixir-rx "~r"
                 (and "<" (group (one-or-more (not (any ">")))) ">"))
     1 font-lock-string-face)

    ;; Modules
    (,(elixir-rx (group module-names))
     1 font-lock-type-face)

    ;; Atoms and singleton-like words like true/false/nil.
    (,(elixir-rx (group atoms)
                 (zero-or-more space)
                 (optional "="))
     1 elixir-atom-face)

    ;; Variable definitions
    (,(elixir-rx (group identifiers)
                 (zero-or-more space)
                 (repeat 1 "=")
                 (or (or sigils identifiers space)
                     (one-or-more "\n")))
     1 font-lock-variable-name-face)

    ;; Map keys
    (,(elixir-rx (group (and (one-or-more identifiers) ":")))
     1 elixir-atom-face)

    ;; Pseudovariables
    (,(elixir-rx (group pseudo-var))
     1 font-lock-constant-face)

    ;; Code points
    (,(elixir-rx (group code-point))
     1 elixir-negation-face)))

;;;###autoload
(defun elixir-mode-open-modegithub ()
  "Elixir mode open GitHub page."
  (interactive)
  (browse-url "https://github.com/elixir-lang/emacs-elixir"))

;;;###autoload
(defun elixir-mode-open-elixir-home ()
  "Elixir mode go to language home."
  (interactive)
  (browse-url elixir-mode-website-url))

;;;###autoload
(defun elixir-mode-open-docs-master ()
  "Elixir mode go to master documentation."
  (interactive)
  (browse-url (concat elixir-mode-website-url "/docs/master/elixir")))

;;;###autoload
(defun elixir-mode-open-docs-stable ()
  "Elixir mode go to stable documentation."
  (interactive)
  (browse-url (concat elixir-mode-website-url "/docs/stable/elixir")))

;;;###autoload
(defun elixir-mode-version (&optional show-version)
  "Get the Elixir-Mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'elixir-mode)))
    (when show-version
      (message "Elixir-Mode version: %s" version))
    version))

(defun elixir-mode-fill-doc-string ()
  (interactive)
  (save-excursion
    (re-search-backward "@\\(?:module\\)?doc +\"\"\"" nil t)
    (re-search-forward "\"\"\"" nil t)
    (set-mark (point))
    (re-search-forward "\"\"\"" nil t)
    (re-search-backward "^ *\"\"\"" nil t)
    (backward-char)
    (fill-region (point) (mark))))

(easy-menu-define elixir-mode-menu elixir-mode-map
  "Elixir mode menu."
  '("Elixir"
    ["Indent line" smie-indent-line]
    "---"
    ["elixir-mode on GitHub" elixir-mode-open-modegithub]
    ["Elixir homepage" elixir-mode-open-elixirhome]
    ["About" elixir-mode-version]))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  "Major mode for editing Elixir code.

\\{elixir-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(elixir-font-lock-keywords))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'tab-width) elixir-basic-offset)
  (set (make-local-variable 'syntax-propertize-function)
       #'elixir-syntax-propertize-function)
  (set (make-local-variable 'imenu-generic-expression)
       elixir-imenu-generic-expression)
  (smie-setup elixir-smie-grammar 'verbose-elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token))

;; Invoke elixir-mode when appropriate

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode)))

(provide 'elixir-mode)

;;; elixir-mode.el ends here
