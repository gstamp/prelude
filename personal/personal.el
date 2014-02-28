;;; personal.el --- Personal changes to prelude
;;; Commentary:

;; Here are the definitions of most of my personalizations to prelude.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Some sane defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I like arrow keys turned on
(setq prelude-guru nil)

;; No thanks to flyspell mode being on by default
(setq prelude-flyspell nil)

;; Auto cleans whitespace
(setq prelude-whitespace t)

;; JS indent levels
(setq js-indent-level 2)
(setq js2-basic-offset 2)

;; By default, Emacs inserts tabs in place of multiple spaces when it
;; formats a region. We want spaces.
(setq-default intent-tabs-mode nil)

;; Setup flx (https://github.com/lewang/flx)
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Indenting defaults
(setq nxml-child-indent 4)
(setq lua-indent-level 4)

;; Allow emacs to access the x clipboard
(setq x-select-enable-clipboard t)

;; Set the GC limit to 40MB to avoid over GCage.
(setq gc-cons-threshold 40000000)

;;  The variable vc-follow-symlinks controls what Emacs does if you
;;  try to visit a symbolic link pointing to a version-controlled
;;  file.
(setq vc-follow-symlinks t)

;; flash instead of ding
(setq visible-bell t)

;; enable fuzzy matching
(setq ido-enable-flex-matching t)

;; Put backups in ~/.emacsbackup/ and control how many versions are kept
(setq backup-directory-alist
      (list (cons ".*" (expand-file-name "~/.emacsbackup/"))))
(setq version-control t)                ; backup multiple versions
(setq delete-old-versions t)            ; delete the older ones
(setq kept-new-versions 10)             ; keep x new ones
(setq kept-old-versions 3)              ; keep x old ones

;; Sets up whitepsace.
;;  trailing - no trailing whitespace and end of line
;;  lines - lines whose have columns beyond ‘whitespace-line-column’
;;          are highlighted via faces.
;;  tabs - TABs are visualized via faces.
(setq whitespace-style '(trailing lines tabs)
      whitespace-line-column 80)        ; no trailing space or tabs

;; gerkin config
(setq feature-default-language "en")

;; Display/redraw defaults
(setq set-cursor-type 'box)
(set-cursor-color "yellow")

;; Can't see a good reason not to enable these commands
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Emacs starter kit turns on flyspell by default but I prefer it off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Redraw more frequently
(setq redisplay-dont-pause 't)

;; Add some extra snippets
;;GJS: (require 'yasnippet)
;;GJS: (setq yas/snippet-dirs
;;GJS:       '("~/.emacs.d/snippets"))
;;GJS: (yas/global-mode 1)

;; Stop ERC telling me about all those people joining/quiting.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Increase find file in project file limit
(setq ffip-limit 3500)

(setq feature-cucumber-command "bundle exec cucumber {options} {feature}")

(when (equal system-type 'darwin)
  (progn
    ;; map meta to the command key on mac
    (setq mac-option-key-is-meta nil)
    (setq mac-left-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)

    (global-set-key [end] 'end-of-line)

    ;; I like the right option key to be control because there's no
    ;; right control key on the mac
    (setq mac-right-option-modifier 'control)

    ;; Option as super
    (setq mac-option-modifier 'super)

    ;; sets fn-delete to be right-delete
    (global-set-key [kp-delete] 'delete-char)

    ;; keybinding to toggle full screen mode
    (global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

    ;; Move to trash when deleting stuff
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs")

    (setenv "LANG" "en_AU.UTF-8")

    (defadvice ansi-term (after advise-ansi-term-coding-system)
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (ad-activate 'ansi-term)
    (defadvice multi-term (after advise-multi-term-coding-system)
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (ad-activate 'multi-term)

    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)

    ;; mac friendly font
    (if window-system
        (progn
          (prelude-require-packages '(exec-path-from-shell))

          (exec-path-from-shell-initialize)
          (set-face-attribute 'default nil :font "Monaco-14")
          (set-frame-size (selected-frame) 124 40)))))

;; Open the buffer list in the same window
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "COMMIT_EDITMSG")
(add-to-list 'same-window-buffer-names "*magit-process*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Window Rearrangement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u)))))

(define-key global-map (kbd "C-|") 'toggle-windows-split)

(defun toggle-split-direction ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key [(shift f9)] 'toggle-split-direction)

(defun split-window-right-and-choose-last-buffer ()
  "Like split-window-right but selects the last buffer"
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-next-buffer)
  (other-window -1))

(global-set-key (kbd "C-x 3") 'split-window-right-and-choose-last-buffer)

(defun split-window-below-and-choose-last-buffer ()
  "Like split-window-below but selects the last buffer"
  (interactive)
  (split-window-below)
  (other-window 1)
  (switch-to-next-buffer)
  (other-window -1))

(global-set-key (kbd "C-x 2") 'split-window-below-and-choose-last-buffer)

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(global-set-key [(shift f10)] 'rotate-windows)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Buffer Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup: The silver surfer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-ensure-module-deps '(ag wgrep-ag))

(global-set-key [f2] 'ag-project-regexp)
(global-set-key [(meta f2)] 'ag-project)

(setq ag-arguments (list "--ignore-case" "--nogroup" "--column" "--"))

;; Automatically save buffers when exiting wgrep edit mode.
(setq wgrep-auto-save-buffer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Editor helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun align-to-colon (begin end)
  "Align region to colon (:) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))

(defun align-to-comma (begin end)
  "Align region to comma signs"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(defun align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(global-set-key [(control shift j)] 'prelude-top-join-line)

(defalias 'iwb 'prelude-indent-buffer)

(global-set-key [home] 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defun mark-line-or-next ()
  "Marks the current line or extends the mark if there is no current selection"
  (interactive)
  (if mark-active
      (forward-line)
    (progn
      (beginning-of-line)
      (push-mark (point))
      (end-of-line)
      (forward-char)
      (activate-mark))))

(global-set-key (kbd "C-;") 'mark-line-or-next)

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stops the mini buffer when switching back to Emacs with mouse
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(defun ert-run ()
  "Evaluate the current buffer and run ert testing framework"
  (interactive)

  (eval-buffer)
  (ert 't)
  )
(global-set-key [f5] 'ert-run)

(defun url-humanify ()
  "Take the URL at point and make it human readable."
  (interactive)
  (let* ((area (bounds-of-thing-at-point 'url))
         (num-params  (count-occurances-in-region "&" (car area) (cdr area)))
         (i 0))
    (beginning-of-thing 'url)
    (when (search-forward "?" (cdr area) t nil)
      (insert "\n  ")
      (while (< i num-params)
        (search-forward "&" nil t nil)
        (insert "\n  ")
        (save-excursion
          (previous-line)
          (beginning-of-line)
          (let ((start (search-forward "="))
                (end (search-forward "&")))
            (url-decode-region start end)))
        (setq i (+ i 1))))))

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun count-occurances-in-region (needle start end)
  (save-excursion
    (let ((found 0))
      (goto-char start)
      (while (search-forward needle end t nil)
        (setq found (+ found 1)))
      found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Git Gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(git-gutter git-gutter-fringe))

(require 'git-gutter-fringe)

(global-git-gutter-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'highlight-symbol)

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f10] 'multi-term)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f4)] 'kill-this-buffer)
(global-set-key [(control f5)] 'linum-mode)
(global-set-key [(control f6)] 'highlight-symbol-at-point)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)


(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; Selects the definition which encloses the point
(global-set-key (kbd "C-M-h") 'mark-defun)

(global-set-key "\r" 'newline-and-indent)

(global-set-key (kbd "C-o") 'prelude-smart-open-line)
(global-set-key (kbd "C-S-o") 'prelude-smart-open-line-above)

;; Repeat (default of C-x z is very hard to type)
(global-set-key (kbd "s-.") 'repeat)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (line-move 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (line-move -5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; This adds an extra keybinding to interactive search (C-s) that runs
;; occur on the current search string/regexp, immediately showing all
;; hits in the entire buffer.  I use it all the time now.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(global-set-key (kbd "M-g M-i") 'prelude-indent-defun)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key isearch-mode-map (kbd "C-o")
  (lambda nil (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido defaults
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

(prelude-require-packages '(ido-vertical-mode))

(ido-vertical-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Golden ratio plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(golden-ratio))

(require 'golden-ratio)

(golden-ratio-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Fiplr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For finding files in a project

(prelude-require-packages '(fiplr))

(setq *grizzl-read-max-results* 20)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "vendor" "tmp"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Helm/Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq projectile-completion-system 'grizzl)

;; Appears to be buggy for me so switch off
(setq projectile-rails-expand-snippet nil)

(global-set-key (kbd "C-x o") 'projectile-find-file)
(global-set-key (kbd "C-x C-o") 'projectile-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Bookmark Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'bm)

;; Part of bookmarks plugin.  Only highlight bookmarks on fringe.
(setq bm-highlight-style 'bm-highlight-only-fringe)

(global-set-key [(shift f2)] 'bm-toggle)
(global-set-key [(shift f3)] 'bm-next)
(global-set-key [(shift f4)] 'bm-prev)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Multiple Cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'multiple-cursors)

(eval-after-load 'multiple-cursors-autoloads
  '(progn

     (key-chord-define-global ";l" 'mc/edit-lines)
     (key-chord-define-global ";e" 'mc/edit-ends-of-lines)
     (key-chord-define-global ";a" 'mc/edit-beginnings-of-lines)

     (global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)
     (global-set-key (kbd "C-<") 'mc/mark-previous-symbol-like-this)
     (global-set-key (kbd "C-*") 'mc/mark-all-dwim)
     (global-unset-key (kbd "M-<down-mouse-1>"))
     (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't use the standard org keys for todo and priority management
;; http://orgmode.org/manual/Conflicts.html
(setq org-replace-disputed-keys 1)
(setq org-confirm-babel-evaluate nil)

;; Colour org mode source code
(setq org-src-fontify-natively 1)
;; Valid task states in org mode
;; Shift left/right switches between modes in the current sequence.
;; Control shift left/right switches to a different sequence.
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE")
        (sequence "ONHOLD" "|" "CANCELLED")))
;; When a task is finished log when it's done
(setq org-log-done 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (sh . t)
   (js . t)
   (java . t)
   (awk . t)
   (sql . t)
   (ruby . t)
   ))

;; Store an org mode link C-cC-l to use it.
(define-key org-mode-map "\C-cl" 'org-store-link)

(global-set-key [f9] 'org-agenda)

;; Slightly nicer default style for exports
(setq org-export-html-style-extra "<style type=\"text/css\">\n  html {\n  font-family: sans-serif;\n  font-size: 11pt;\n  }\n  em { font-style: normal; font-weight: bold;}\n</style>")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(clojure-mode cider align-cljlet))

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet))

;; Save buffer before trying to compile
(defadvice cider-load-current-buffer (before save-before-cider-compile activate compile)
  (save-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(rinari rspec-mode bundler ruby-mode ruby-tools company company-inf-ruby ruby-hash-syntax ruby-refactor projectile-rails rubocop))

(setq rubocop-keymap-prefix (kbd "C-c C-c"))
(setq projectile-rails-keymap-prefix (kbd "C-c C-f"))

;; Projecttile rails setup
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; 'Fix' 'WARNING: terminal is not fully functional' from less/etc.
(setenv "PAGER" "cat")

(defun rspec-run-and-arrange ()
  (interactive)

  (save-buffer)
  (if (not (rspec-buffer-is-spec-p))
      (rspec-toggle-spec-and-target))

  (rspec-verify)
  (delete-other-windows)
  (if (get-buffer "*rspec-compilation*")
      (let ((result-window (split-window-below)))

        (set-window-buffer result-window "*rspec-compilation*")
        (rspec-toggle-spec-and-target)

        (split-window-right-and-choose-last-buffer)

        (enlarge-window 10)
        )))

(eval-after-load 'ruby-tools
  '(progn
     (define-key ruby-tools-mode-map (kbd "C-;") nil)
     (define-key ruby-tools-mode-map (kbd "C-c C-.") 'ruby-tools-clear-string)
     ))

(add-hook 'ruby-mode-hook
          (lambda ()
            (ruby-refactor-mode-launch)
            (define-key ruby-mode-map (kbd "M-\"") 'rspec-run-and-arrange)
            ;; RSpec settings for greater good.
            (setq rspec-use-rake-flag nil)
            (setq rspec-use-bundler-when-possible 't)))

(setq ruby-refactor-add-parens +1)

(defun ruby-convert-json-to-hash (startPos endPos)
  "Convert a json object to a ruby hash..
This command calls the external script 'convert_json_to_rb_hash.rb'."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "convert_json_to_rb_hash.rb") ; full path to your script
    (shell-command-on-region startPos endPos scriptName nil t nil t))
  (indent-region startPos endPos))

(defun ruby-convert-hash-to-json (startPos endPos)
  "Convert a json object to a ruby hash..
This command calls the external script 'convert_json_to_rb_hash.rb'."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "convert_rb_hash_to_json.rb")
    (shell-command-on-region startPos endPos scriptName nil t nil t))
  (indent-region startPos endPos))

(defun ruby-eval-region()
  "Prints the evaluation of Ruby statements in region to a new output buffer"
  (interactive)
  (let ((output-buffer "Ruby Output"))
    (shell-command-on-region (mark) (point) "ruby" output-buffer)
    (switch-to-buffer output-buffer)))

(defun ruby-awesome-print (start-pos end-pos)
  "Use awesome print to format the selected bit of code"
  (interactive "r")
  (let (ruby-script
        script-name)
    (setq ruby-script (concat "require 'awesome_print'\nap(eval(ARGF.read), indent: -2, index: false)\n"))
    (setq script-name (concat "ruby -e \"" ruby-script "\""))
    (shell-command-on-region start-pos end-pos script-name nil t nil t))
  (indent-region start-pos end-pos))




;; Tell Riniari about extra prompt patterns
;; (setq rinari-inf-ruby-prompt-pattern
;;       (concat rinari-inf-ruby-prompt-pattern "\\|\\(.*»\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq magit-status-buffer-switch-function 'switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Dash and point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(dash-at-point))

(global-set-key [(shift f1)] 'dash-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(web-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-hook 'web-mode-hook (lambda ()
                           (font-lock-mode -1)
                           (smartparens-mode 0)
                           (setq web-mode-code-indent-offset 2)   ;; prelude sets this to 4
                           (setq web-mode-markup-indent-offset 2) ;; prelude sets this to 4
                           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Org Drill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-learn)
(require 'org-drill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Smart scan. Use M-n M-p to move forward and backward
;;;;        for symbol at point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")

(defvar smart-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")

(make-local-variable 'smart-use-extended-syntax)

(defvar smart-symbol-old-pt nil
  "Contains the location of the old point")

(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (unless (catch 'done
            (while (funcall (cond
                             ((eq direction 'forward) ; forward
                              'search-forward)
                             ((eq direction 'backward) ; backward
                              'search-backward)
                             (t (error "Invalid direction"))) ; all others
                            smart-last-symbol-name nil t)
              (unless (memq (syntax-ppss-context
                             (syntax-ppss (point))) '(string comment))
                (throw 'done t))))
    (goto-char smart-symbol-old-pt)))

(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))

(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))

(defun smart-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.

If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  (with-syntax-table (make-syntax-table)
    (if smart-use-extended-syntax
        (modify-syntax-entry ?. "w"))
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))

(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Pretty Lambdas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enable-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'prog-mode-hook 'enable-pretty-lambdas)

(defun enable-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192") nil))))))
(add-hook 'clojure-mode-hook 'enable-pretty-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: No Bell: Prevent the bell from ringing all the time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; found at http://www.elliotglaysher.org/emacs/

;; TODO(erg): Figure out why that note doesn't appear in the mode-line-bar...
(defcustom mode-line-bell-string "ding" ; "♪"
  "Message displayed in mode-line by `mode-line-bell' function."
  :group 'user)
(defcustom mode-line-bell-delay 0.1
  "Number of seconds `mode-line-bell' displays its message."
  :group 'user)

;; internal variables
(defvar mode-line-bell-cached-string nil)
(defvar mode-line-bell-propertized-string nil)

(defun mode-line-bell ()
  "Briefly display a highlighted message in the mode-line.

The string displayed is the value of `mode-line-bell-string',
with a red background; the background highlighting extends to the
right margin.  The string is displayed for `mode-line-bell-delay'
seconds.

This function is intended to be used as a value of `ring-bell-function'."

  (unless (equal mode-line-bell-string mode-line-bell-cached-string)
    (setq mode-line-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(string-width mode-line-bell-string))))
            mode-line-bell-string)
           'face '(:background "black")))
    (setq mode-line-bell-cached-string mode-line-bell-string))
  (message mode-line-bell-propertized-string)
  (sit-for mode-line-bell-delay)
  (message ""))

(setq ring-bell-function 'mode-line-bell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: YaSnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(yasnippet))
(setq yas/snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Highlight indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(highlight-indentation))

(add-hook 'ruby-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'coffee-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(custom-set-faces
 '(highlight-indentation-current-column-face ((t (:background "#444444")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Auto reload tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-parent-tags (dir)
  "Traverses the directory tree up to /home/[user]/ or / whichever comes first.
   Returns either nil or the directory containing the first TAGS file it finds."
  (interactive (list default-directory))
  (find-parent-tags-rec (build-tag-paths dir)))

(defun find-parent-tags-rec (list-of-filepath)
  (cond ((null list-of-filepath) nil)
        ((and (file-exists-p (car list-of-filepath)) (file-regular-p (car list-of-filepath))) (car list-of-filepath))
        (t (find-parent-tags-rec (cdr list-of-filepath)))))

(defun build-tag-paths (dir-string)
  (build-tag-paths-rec (remove-if #'empty-string? (split-string dir-string "/")) (list "/")))

(defun build-tag-paths-rec (steps acc)
  (if (null steps)
      (mapcar (lambda (p) (concat p "TAGS")) acc)
    (build-tag-paths-rec (cdr steps)
                         (cons (concat (car acc) (car steps) "/") acc))))

(defun empty-string? (s) (equalp s ""))

(defun auto-find-tagfile ()
  "Automatically find the tag file and load."
  (interactive)

  (visit-tags-table (find-parent-tags default-directory))
  (message "Tag file reloaded"))

(global-set-key (kbd "M-<f9>") 'auto-find-tagfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Code Wrap Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode code-wrap-mode
	"Visually wrap the buffer text to the previous lines indent, plus a tab."
	:lighter ""
	(if code-wrap-mode
		(progn
			(visual-line-mode 1)
			(jit-lock-register #'indent-prefix-wrap-function))
		(visual-line-mode 0)
		(jit-lock-unregister #'indent-prefix-wrap-function)
		(with-silent-modifications
			(save-restriction
				(widen)
				(remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(setq indent-prefix-extra "\t")
(defun indent-prefix-wrap-function (&optional beg end)
	"Indent the region between BEG and END with the existing indent, plus a little extra."
	(when (and beg end)
		(let ((tab-as-spaces (string-repeat " " tab-width)))
			(goto-char beg)
			(beginning-of-line)
			(while (< (point) end)
				(let* ((line-start (point))
					   (indent-start (progn (skip-syntax-forward " " end) (point)))
					   (line-end (progn (beginning-of-line 2) (- (point) 1)))
					   (prefix (concat (buffer-substring line-start indent-start) indent-prefix-extra)))
					;; replace tabs with spaces because the tab indent gets messed up
					(setq prefix (replace-regexp-in-string "\t" tab-as-spaces prefix))
					;; if we are indenting more than half the window width, give up and go back to a one-tab indent
					(when (> (length prefix) (/ (window-width) 2))
						(setq prefix tab-as-spaces))
					(put-text-property line-start line-end 'wrap-prefix prefix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(cucumber-goto-step visual-regexp discover))

(define-key global-map (kbd "M-g M-r") 'vr/replace)
(define-key global-map (kbd "M-g r") 'vr/query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: IMenu Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When invoking imenu this creates a 'Sections' entry that you can
;; use to jump between sections in this file.  A section is any
;; comment starting with 4 semicolons.
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'personal)
;;; personal.el ends here

