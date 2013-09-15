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

;; TODO: Not sure this is actually doing anything
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

;; Emacs starter kit also turns on idle-highlight-mode.  Not a fan.
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

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
        (set-face-attribute 'default nil :font "Monaco-12"))
    ))

;; Open the buffer list in the same window
(add-to-list 'same-window-buffer-names "*Buffer List*")

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
;; Setup: Ack and a Half
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-ensure-module-deps '(ack-and-a-half))

(eval-after-load 'ack-and-a-half
  '(progn
     (setq ack-and-a-half-root-directory-functions 'ack-and-a-half-guess-project-root)))

(global-set-key [f2] 'ack-and-a-half)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Editor helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(defun join-with-next-line ()
  "join with next line"
  (interactive)
  (next-line)
  (delete-indentation))

(global-set-key [(control shift j)] 'join-with-next-line)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; TODO: setup a keybinding for this


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Make open-line work more like VI (bound to ctrl-o)
(defadvice open-line (before new-open-line activate)
  (end-of-visible-line))
(defadvice open-line (after after-open-line activate)
  (forward-line 1)
  (indent-according-to-mode))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (if (not (string-equal major-mode "org-mode"))
      (let ((oldpos (point)))
        (back-to-indentation)
        (and (= oldpos (point))
             (beginning-of-line)))
    (move-beginning-of-line nil)))

(global-set-key [home] 'smart-beginning-of-line)

;; this will indent the yanked region automatically in the provided
;; modes
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode
                                           lisp-mode
                                           clojure-mode
                                           ruby-mode
                                           c-mode
                                           c++-mode
                                           objc-mode
                                           LaTeX-mode
                                           TeX-mode))
      (indent-region (region-beginning) (region-end) nil)))

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

;; Stops the mini buffer when switching back to emacs with mouse
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

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Git Gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(git-gutter+ git-gutter-fringe+))

(global-git-gutter+-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f10] 'multi-term)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(control f4)] 'kill-this-buffer)
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key [(control f5)] 'linum-mode)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; Selects the definition which encloses the point
(global-set-key (kbd "C-M-h") 'mark-defun)

(global-set-key "\r" 'newline-and-indent)

(global-set-key (kbd "<S-return>") 'open-line)
(global-set-key (kbd "C-S-o") '"\C-p\C-o") ; open line above

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; This adds an extra keybinding to interactive search (C-s) that runs
;; occur on the current search string/regexp, immediately showing all
;; hits in the entire buffer. I use it all the time now.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido defaults
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2)

;; Make ido open file list vertically
(setq ido-max-prospects 60)
(setq ido-max-file-prompt-width 0.8)
(setq ido-max-window-height 30)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(setq ffip-full-paths 't) ; Not really part of ido but works well with
                                        ; vertically displayed lists.

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

(setq *grizzl-read-max-results* 18)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "vendor" "tmp"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x o") 'fiplr-find-file)
(global-set-key (kbd "C-x C-o") 'fiplr-find-file)

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
;;;; Setup: Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'paredit)

(add-hook 'emacs-lisp-mode-hook                   #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook  #'enable-paredit-mode)
(add-hook 'clojure-mode-hook                      #'enable-paredit-mode)

(eval-after-load 'paredit
  '(progn

     ;; making paredit work with delete-selection-mode
     (put 'paredit-forward-delete 'delete-selection 'supersede)
     (put 'paredit-backward-delete 'delete-selection 'supersede)
     (put 'paredit-open-round 'delete-selection t)
     (put 'paredit-open-square 'delete-selection t)
     (put 'paredit-doublequote 'delete-selection t)
     (put 'paredit-newline 'delete-selection t)

     ;; Paredit improvements pinched from emacs-live
     (defun live-paredit-next-top-level-form ()
       (interactive)
       (while (ignore-errors (paredit-backward-up) t))
       (live-paredit-forward))

     (defun live-paredit-previous-top-level-form ()
       (interactive)
       (if (ignore-errors (paredit-backward-up) t)
           (while (ignore-errors (paredit-backward-up) t))
         (paredit-backward)))

     (defun live-paredit-forward ()
       "Feels more natural to move to the beginning of the next item
in the sexp, not the end of the current one."
       (interactive)
       (if (and (not (paredit-in-string-p))
                (save-excursion
                  (ignore-errors
                    (forward-sexp)
                    (forward-sexp)
                    t)))
           (progn
             (forward-sexp)
             (forward-sexp)
             (backward-sexp))
         (paredit-forward)))

     (defun live-paredit-reindent-defun (&optional argument)
       "Reindent the definition that the point is on. If the point is
  in a string or a comment, fill the paragraph instead, and with
  a prefix argument, justify as well. Doesn't mess about with
  Clojure fn arglists when filling-paragraph in docstrings."
       (interactive "P")
       (cond ((paredit-in-comment-p) (fill-paragraph argument))
             ((paredit-in-string-p) (progn
                                      (save-excursion
                                        (paredit-forward-up)
                                        (insert "\n"))
                                      (fill-paragraph argument)
                                      (save-excursion
                                        (paredit-forward-up)
                                        (delete-char 1))))
             (t (save-excursion
                  (end-of-defun)
                  (beginning-of-defun)
                  (indent-sexp)))))

     (defun live-paredit-forward-kill-sexp (&optional arg)
       (interactive "p")
       (cond ((or (paredit-in-comment-p)
                  (paredit-in-string-p)) (kill-word (or arg 1)))
             (t (kill-sexp (or arg 1)))))

     (defun live-paredit-backward-kill-sexp (&optional arg)
       (interactive "p")
       (cond ((or (paredit-in-comment-p)
                  (paredit-in-string-p)) (backward-kill-word (or arg 1)))
             (t (backward-kill-sexp (or arg 1)))))

     (defun paredit--is-at-start-of-sexp ()
       (and (looking-at "(\\|\\[")
            (not (nth 3 (syntax-ppss))) ;; inside string
            (not (nth 4 (syntax-ppss))))) ;; inside comment

     (defun paredit-duplicate-closest-sexp ()
       (interactive)
       ;; skips to start of current sexp
       (while (not (paredit--is-at-start-of-sexp))
         (paredit-backward))
       (set-mark-command nil)
       ;; while we find sexps we move forward on the line
       (while (and (bounds-of-thing-at-point 'sexp)
                   (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                   (not (= (point) (line-end-position))))
         (forward-sexp)
         (while (looking-at " ")
           (forward-char)))
       (kill-ring-save (mark) (point))
       ;; go to the next line and copy the sexprs we encountered
       (paredit-newline)
       (yank)
       (exchange-point-and-mark))

     (defun paredit-wrap-round-from-behind ()
       (interactive)
       (forward-sexp -1)
       (paredit-wrap-round)
       (insert " ")
       (forward-char -1))

     ;; Some paredit keybindings conflict with windmove and SLIME,
     ;; adjust those and make some new bindings.
     (define-key paredit-mode-map (kbd "<C-left>") nil)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map "\M-r" nil)
     (define-key paredit-mode-map (kbd "C-M-f") 'live-paredit-forward)
     (define-key paredit-mode-map (kbd "C-M-k") 'live-paredit-forward-kill-sexp)
     (define-key paredit-mode-map (kbd "C-M-<backspace>") 'live-paredit-backward-kill-sexp)
     (define-key paredit-mode-map (kbd "M-q") 'live-paredit-reindent-defun)
     (define-key paredit-mode-map (kbd "M-<up>") 'live-paredit-previous-top-level-form)
     (define-key paredit-mode-map (kbd "M-<down>") 'live-paredit-next-top-level-form)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
     (define-key paredit-mode-map (kbd "C-S-d") 'paredit-duplicate-closest-sexp)

     ))

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
;;;; Setup: Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq magit-status-buffer-switch-function 'switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'personal)
;;; personal.el ends here
