;;; personal.el --- Personal changes to prelude
;;; Commentary:

;; Here are the definitions of most of my personalizations to prelude.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Some sane defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I like arrow keys turned on
(setq prelude-guru nil)

;; Ignore the case when sorting
(setq sort-fold-case t)

;; Use GPG for *.gpg files and *.asc files
(setq epa-file-name-regexp "\\(\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'\\)\\|\\(\\.asc\\(~\\|\\.~[0-9]+~\\)?\\'\\)")
(epa-file-name-regexp-update)

;; No thanks to flyspell mode being on by default
(setq prelude-flyspell nil)

;; Don't auto clean whitespace, we'll let ws-butler deal with it
(setq prelude-clean-whitespace-on-save nil)

;; Show whitespace
(setq prelude-whitespace nil)

;; JS indent levels
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)

;; Don't ask about the tag table.
(setq tags-add-tables nil)

;; By default, Emacs inserts tabs in place of multiple spaces when it
;; formats a region. We want spaces.
(setq-default intent-tabs-mode nil)

(ido-everywhere 1)

;; Indenting defaults
(setq nxml-child-indent 4)
(setq lua-indent-level 4)

;; Why you keep prompting me emacs?
(setq tags-revert-without-query 1)

;; Allow emacs to access the x clipboard
(setq x-select-enable-clipboard t)

;;  The variable vc-follow-symlinks controls what Emacs does if you
;;  try to visit a symbolic link pointing to a version-controlled
;;  file.
(setq vc-follow-symlinks t)

;; Ignore version control for tramp
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

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

(setq whitespace-line-column 80)

;; Sets up whitepsace.
;;  trailing - no trailing whitespace and end of line
;;  lines - lines whose have columns beyond ‘whitespace-line-column’
;;          are highlighted via faces.
;;  tabs - TABs are visualized via faces.
;;  newline-mark - NEWLINEs are visualized via display table.
;;  tab-mark - TABs are visualized via display table.
(setq whitespace-style (quote (tabs newline trailing lines tab-mark newline-mark)))

;; Unicode for whitespace thingies
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

;; Normally prelude only enables whitespace cleanup when whitespace
;; mode is switched on. Best to have it on all the time.
(add-hook 'before-save-hook 'prelude-cleanup-maybe nil nil)

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

(setq ag-reuse-buffers t)

;; Redraw more frequently
(setq redisplay-dont-pause 't)

;; Allow editing of permissions in writable-dired-mode
(setq wdired-allow-to-change-permissions 't)

;; Stop ERC telling me about all those people joining/quiting.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Increase find file in project file limit
(setq ffip-limit 3500)

(setq feature-cucumber-command "bundle exec cucumber {options} {feature}")

;; Why are we setting the shell explicitly? I'm glad you
;; asked. zsh has this habit of prepending standard paths
;; onto the PATH environment variable. This means we
;; frequently end up using the wrong version of ruby.
(setq shell-file-name "/bin/bash")

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

    ;; Move to trash when deleting stuff
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs")

    (setenv "LANG" "en_AU.UTF-8")

    ;; mac friendly font
    (if window-system
        (progn
          (prelude-require-packages '(exec-path-from-shell))


          (setq exec-path-from-shell-variables '("PATH"
                                                 "MANPATH"
                                                 "BOXEN_HOME"
                                                 "BOXEN_BIN_DIR"
                                                 "BOXEN_CONFIG_DIR"
                                                 "BOXEN_DATA_DIR"
                                                 "BOXEN_ENV_DIR"
                                                 "BOXEN_LOG_DIR"
                                                 "BOXEN_SOCKET_DIR"
                                                 "BOXEN_SRC_DIR"
                                                 "BOXEN_DOWNLOAD_URL_BASE"
                                                 "BOXEN_HOMEBREW_BOTTLE_URL"
                                                 "BOXEN_GITHUB_LOGIN"
                                                 "BOXEN_ELASTICSEARCH_HOST"
                                                 "BOXEN_ELASTICSEARCH_PORT"
                                                 "BOXEN_ELASTICSEARCH_URL"
                                                 "BOXEN_MONGODB_HOST"
                                                 "BOXEN_MONGODB_PORT"
                                                 "BOXEN_MONGODB_URL"
                                                 "BOXEN_POSTGRESQL_HOST"
                                                 "BOXEN_POSTGRESQL_PORT"
                                                 "BOXEN_POSTGRESQL_URL"
                                                 "BOXEN_REDIS_HOST"
                                                 "BOXEN_REDIS_PORT"
                                                 "BOXEN_REDIS_URL"
                                                 "BOXEN_MEMCACHED_PORT"
                                                 "BOXEN_MEMCACHED_URL"
                                                 "BOXEN_MYSQL_PORT"
                                                 "BOXEN_MYSQL_SOCKET"
                                                 "BOXEN_MYSQL_URL"
                                                 "BOXEN_SETUP_VERSION"
                                                 "RBENV_ROOT"
                                                 "BUNDLE_JOBS"
                                                 "GOPATH"))

          (exec-path-from-shell-initialize)

          ))))

;; Open the buffer list in the same window
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "COMMIT_EDITMSG")
(add-to-list 'same-window-buffer-names "*magit-process*")
(add-to-list 'same-window-buffer-names "*Org Agenda*")

;; Don't auto add newline at EOF
(setq require-final-newline nil)
(setq next-line-add-newlines nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

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

;; Auto save on focus change
(defun save-all ()
  (interactive)
  (simple-save-some-buffers))
(add-hook 'focus-out-hook 'save-all)

;; ibuffer-vc setup
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

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

(global-set-key [(control f2)] 'projectile-ag)
(global-set-key [(meta f2)] 'ag-project)

(setq ag-arguments (list "--smart-case" "--stats"))

;; Automatically save buffers when exiting wgrep edit mode.
(setq wgrep-auto-save-buffer t)

;; Setup writable ag mode
;; C-c C-p to enter edit mode
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Editor helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))
(global-set-key (kbd "<M-return>") 'new-line-dwim)

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))
;; TODO: Find good keybind for this

(defun auto-align (align-to mark-f)
  (save-excursion
    (unless mark-active
      (funcall mark-f))
    (align-regexp (min (point) (mark))
                  (max (point) (mark))
                  align-to 1 1 )))

(defun align-to-equals ()
  "Align region to equal signs"
  (interactive)
  (auto-align "\\(\\s-*\\)=" #'er/mark-paragraph))

(defun align-to-colon ()
  "Align region to colon (:) signs"
  (interactive)
  (auto-align ":\\(\\s-*\\)" #'er/mark-inside-pairs))

(defun align-to-comma ()
  "Align region to comma signs"
  (interactive)
  (auto-align ",\\(\\s-*\\)" #'er/mark-inside-pairs))

(defun align-to-hash ()
  "Align region to hash ( => ) signs automatically selecting scope if no region active"
  (interactive)
  (auto-align "\\(\\s-*\\)=>" #'er/mark-inside-pairs))

(defadvice shell-command (before my-shell-command (command &optional output-buffer error-buffer) activate)
  "Save the buffer before runng shell command"
  (simple-save-some-buffers))

;; (defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
;;   "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
;;   you are deleting forward, the CHAR is replaced and the point is
;;   put before CHAR"
;;   (insert char)
;;   (if (< 0 arg) (forward-char -1)))

(defun select-method()
  (local-set-key (kbd "M-h") #'mark-defun))

(add-hook 'prog-mode-hook 'select-method)

(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(global-set-key [(control shift j)] 'crux-top-join-line)

(global-set-key [home] (kbd "C-a"))

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

;; Ask to create the directory if it doesn't exist
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

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

(defun diff-current-with-master ()
  (interactive)
  (shell-command (format "git diff master -- %s" (buffer-file-name))) nil)

(global-set-key [f8] 'diff-current-with-master)

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”."
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `user-buffer-q'."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `user-buffer-q'."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun kill-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))
(global-set-key (kbd "C-c C-k") 'kill-dired-buffers)

;; If inside a string escape the pasted contents. (consider making default)
(defun escape-yank()
  (interactive)
  (if (nth 3 (syntax-ppss)) ;; Checks if inside a string
      (insert-for-yank (replace-regexp-in-string "[\\\"]"
                                                 "\\\\\\&"
                                                 (current-kill 0)
                                                 t))
    (call-interactively 'yank)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun ediff-marked-pair ()
  "Run ediff-files on a pair of files marked in dired buffer"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          (t (error "mark exactly 2 files, at least 1 locally")))))


;; Modifed version of save-some-buffers that saves the buffers but doesn't
;; spit out rubbish to the message bage
(defun simple-save-some-buffers ()
  (interactive)
  (save-window-excursion
    (let* (queried autosaved-buffers
                   files-done abbrevs-done)
      (dolist (buffer (buffer-list))
        ;; First save any buffers that we're supposed to save unconditionally.
        ;; That way the following code won't ask about them.
        (with-current-buffer buffer
          (when (and buffer-save-without-query (buffer-modified-p))
            (push (buffer-name) autosaved-buffers)
            (save-buffer))))
      ;; Ask about those buffers that merit it,
      ;; and record the number thus saved.
      (setq files-done
            (mapcar
             (lambda (buffer)
               (with-current-buffer buffer
                 (if (and (buffer-live-p buffer)
                          (buffer-modified-p buffer)
                          (not (buffer-base-buffer buffer))
                          (buffer-file-name buffer)
                          )
                     (save-buffer))))
             (buffer-list)
             ))
      ;; Maybe to save abbrevs, and record whether
      ;; we either saved them or asked to.
      (and save-abbrevs abbrevs-changed
           (progn
             (if (or (eq save-abbrevs 'silently)
                     (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
                 (write-abbrev-file nil))
             ;; Don't keep bothering user if he says no.
             (setq abbrevs-changed nil)
             (setq abbrevs-done t)))
      )))

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
;;;; Setup: Aggressive indent mode for elisp and clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'aggressive-indent)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Clipboard link maker thingo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from https://gist.github.com/rexim/8883151

(defun straight-string (s)
  (mapconcat '(lambda (x) x) (split-string s) " "))

(defun extract-title-from-html (html)
  (let ((start (string-match "<title>" html))
        (end (string-match "</title>" html))
        (chars-to-skip (length "<title>")))
    (if (and start end (< start end))
        (substring html (+ start chars-to-skip) end)
      nil)))

(defun prepare-cliplink-title (title)
  (let ((replace-table '(("\\[" . "{")
                         ("\\]" . "}")
                         ("&mdash;" . "—")))
        (max-length 77)
        (result (straight-string title)))
    (dolist (x replace-table)
      (setq result (replace-regexp-in-string (car x) (cdr x) result)))
    (when (> (length result) max-length)
      (setq result (concat (substring result 0 max-length) "...")))
    result))

(defun perform-cliplink (buffer url content)
  (let* ((decoded-content (decode-coding-string content 'utf-8))
         (title (prepare-cliplink-title
                 (extract-title-from-html decoded-content))))
    (with-current-buffer buffer
      (insert (format "[[%s][%s]]" url title)))))

(defun cliplink ()
  (interactive)
  (let ((dest-buffer (current-buffer))
        (url (substring-no-properties (current-kill 0))))
    (url-retrieve
     url
     `(lambda (s)
        (perform-cliplink ,dest-buffer ,url
                          (buffer-string))))))

;; TODO: Probably want to make this org specific
(global-set-key (kbd "C-c C-g") 'cliplink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'highlight-symbol)

;; (global-set-key [f1] 'ido-switch-buffer)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f6] 'highlight-symbol-at-point)
(global-set-key [(shift f6)] 'unhighlight-regexp)
(global-set-key [M-f10] (quote toggle-frame-fullscreen))
(global-set-key [f10] 'toggle-frame-maximized)
(global-set-key [f11] 'ido-kill-buffer)

(global-set-key [(control f4)] 'kill-this-buffer)
(global-set-key [(control f5)] 'linum-mode)

;; Navigate backward and forward through buffers
(global-set-key [(super left)] 'previous-user-buffer)
(global-set-key [(super right)] 'next-user-buffer)
(defun my-switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key [C-tab] 'my-switch-to-other-buffer)

;; Switch buffer
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Helm recent files
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-recentf)

;; Selects the definition which encloses the point
(global-set-key (kbd "C-M-h") 'mark-defun)

(global-set-key "\r" 'newline-and-indent)

(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "C-S-o") 'crux-smart-open-line-above)

;; Repeat (default of C-x z is very hard to type)
(global-set-key (kbd "s-.") 'repeat)

;; Just use 'e' to enter edit mode
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)
(eval-after-load 'ag
  (lambda ()
    (define-key ag-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)))

;; Makes dired jump up directory when pressing ctrl-l - same as helm
(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (line-move 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (line-move -5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

(global-set-key (kbd "M-g M-i") 'crux-indent-defun)

(global-set-key (kbd "M-x") 'helm-M-x)

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
;;;; Setup: Helm/Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(helm helm-swoop helm-ls-git))

;; Reload the theme as something in the load process messes it up
(load-theme prelude-theme)

;; while in swoop mode C-c C-e to edit
(global-set-key (kbd "C-c C-/") 'helm-swoop)

(global-set-key (kbd "C-x o") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-o") 'helm-projectile-find-file)

;; max size of filename
(setq helm-buffer-max-length 30)

;; Some experimental helm defaults
(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           nil ; Force splitting inside selected window when non--nil.
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; Resize automatically
(helm-autoresize-mode 1)

;; Hide minibuffer with helm
(setq helm-echo-input-in-header-line t)
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)


(global-set-key [f1] 'helm-projectile)
(global-set-key (kbd "C-<f1>") 'helm-ls-git-ls)
(global-set-key [f2] 'helm-projectile-ag)

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

(prelude-require-packages '(multiple-cursors phi-search))

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-dwim)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(org-bullets))

;; Git flavoured markdown
(require 'ox-md)

;; Start up bullets mode when org is loaded.
(with-eval-after-load 'org
  (org-bullets-mode 't))

;; Don't use the standard org keys for todo and priority management
;; http://orgmode.org/manual/Conflicts.html
(setq org-replace-disputed-keys t)
(setq org-confirm-babel-evaluate nil)

;; Inline images displayed by default
(setq org-startup-with-inline-images t)

;; Define org-capture key - make sure org-default-notes-file is
;; defined for local machine
(define-key global-map (kbd "C-c c") 'org-capture)

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

;; Load org agenda as the first screen you see
(add-hook 'after-init-hook 'org-todo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(clojure-mode cider cider-decompile cider-spy clojure-quick-repls cider-eval-sexp-fu clj-refactor))

;; Save buffer before trying to compile
(defadvice cider-load-current-buffer (before save-before-cider-compile activate compile)
  (save-buffer))

;; Don't warn when running refactoring operations
(setq cljr-warn-on-eval nil)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'Fix' 'WARNING: terminal is not fully functional' from less/etc.
(setenv "PAGER" "cat")

(when (equal system-type 'darwin)
  (progn
    (defadvice ansi-term (after advise-ansi-term-coding-system)
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (ad-activate 'ansi-term)
    (defadvice multi-term (after advise-multi-term-coding-system)
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (ad-activate 'multi-term)

    ;; Fixes multiterm problems I'm having
    (setq locale-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    ))

(defun send-to-iterm (str)
  "Send STR to the front window/session in iTerm. STR may contain
multiple lines separated by `\n'."
  (interactive "iTerm input: ")
  (let ((lines (split-string
                (replace-regexp-in-string "\"" "\\\"" str t t)
                "\n")))
    (do-applescript (concat
                     "tell application \"iTerm\"\n"
                     "  tell the current terminal\n"
                     "    tell the current session\n"
                     (mapconcat (lambda (s) (concat "write text \"" s "\"\n")) lines "")
                     "    end tell\n"
                     "  end tell\n"
                     "end tell\n"
                     ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(rinari rspec-mode bundler ruby-mode
                                   ruby-tools ruby-hash-syntax
                                   ruby-refactor projectile-rails
                                   adaptive-wrap
                                   rainbow-identifiers
                                   ))

;; Don't insert crap into my file automatically
(setq ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'rainbow-identifiers-mode)

(setq projectile-rails-keymap-prefix (kbd "C-c C-f"))

;; Align to the tart of the statement rather than the keyword
(setq ruby-align-to-stmt-keywords 't)

;; Enable symbol auto completion
(setq dabbrev-abbrev-skip-leading-regexp ":")

(setq rspec-command-options "--format progress")

;; Projecttile rails setup
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; Avoid smart paren being too clever in ruby mode
(sp-local-pair 'ruby-mode "(" ")" :unless '(sp-point-before-word-p))
(sp-local-pair 'ruby-mode "{" "}" :unless '(sp-point-before-word-p))
(sp-local-pair 'ruby-mode "[" "]" :unless '(sp-point-before-word-p))
(sp-local-pair 'ruby-mode "\"" "\"" :unless '(sp-point-before-word-p))

;; Fix problem with running rspec with zsh
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

;; Nice helper to arrange rspec windows nicely
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

(defun close-rspec-output-buffer()
  (if (get-buffer "*rspec-compilation*")
      (kill-buffer (get-buffer "*rspec-compilation*"))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (visual-line-mode)
            (ruby-refactor-mode-launch)

            ;; Hack rspec so we can close the rspec popup when finished
            (require 'rspec-mode)
            (defun rspec-handle-complete (&rest ignore)
              (save-excursion
                (goto-char (point-max))
                (if (save-excursion
                      (forward-line -15)
                      (search-forward "0 failures" nil t))
                    (progn
                      (popwin:close-popup-window)
                      (message "%s" (propertize "All green. Super!" 'face '(:foreground "green"))))
                  (progn
                    (popwin:select-popup-window)
                    (popwin:one-window)
                    (message "%s" (propertize "Oh noes!!!
                    failures!" 'face '(:foreground "red"))) ))))
            (define-derived-mode rspec-compilation-mode compilation-mode "RSpec Compilation"
              "Compilation mode for RSpec output."
              (set (make-local-variable 'compilation-error-regexp-alist)
                   (append '(rspec rspec-capybara-html rspec-capybara-screenshot)
                           compilation-error-regexp-alist))
              (set (make-local-variable 'compilation-error-regexp-alist-alist)
                   (append '((rspec-capybara-html
                              "Saved file \\([0-9A-Za-z@_./\:-]+\\.html\\)" 1 nil nil 0 1)
                             (rspec-capybara-screenshot
                              "Screenshot: \\([0-9A-Za-z@_./\:-]+\\.png\\)" 1 nil nil 0 1)
                             (rspec
                              "rspec +\\([0-9A-Za-z@_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2 nil 2 1))
                           compilation-error-regexp-alist-alist))
              (setq font-lock-defaults '(rspec-compilation-mode-font-lock-keywords t))
              (add-hook 'compilation-filter-hook 'rspec-colorize-compilation-buffer nil t)
              (add-hook 'compilation-finish-functions 'rspec-handle-error nil t)
              (add-hook 'compilation-finish-functions 'rspec-handle-complete nil t))

            (define-key ruby-mode-map (kbd "S-<f5>") 'rspec-run-and-arrange)
            (define-key ruby-mode-map (kbd "<f5>") (lambda() (interactive) (close-rspec-output-buffer) (rspec-verify)))
            (define-key ruby-mode-map (kbd "M-<f5>") (lambda() (interactive) (close-rspec-output-buffer) (rspec-verify-all)))
            ;; RSpec settings for greater good.
            (setq rspec-use-rake-flag nil)
            (setq rspec-use-bundler-when-possible 't)
            ))

;; Use preludes beginning of line for virual-line-mode
(define-key visual-line-mode-map [remap move-beginning-of-line] 'crux-move-beginning-of-line)

(setq ruby-refactor-add-parens +1)

(defun ruby-convert-json-to-hash (startPos endPos)
  "Convert a json object to a ruby hash..
This command calls the external script 'json-to-ruby'."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "json-to-ruby") ; full path to your script
    (shell-command-on-region startPos endPos scriptName nil t nil t))
  (indent-region startPos endPos))

(defun ruby-convert-hash-to-json (startPos endPos)
  "Convert a json object to a ruby hash..
This command calls the external script 'ruby-to-json.rb'."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "ruby-to-json")
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

;; Make ruby work with hideshow mode
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))


;; Tell Riniari about extra prompt patterns
;; (setq rinari-inf-ruby-prompt-pattern
;;       (concat rinari-inf-ruby-prompt-pattern "\\|\\(.*»\\)"))


;; https://github.com/jimweirich/emacs-setup-esk/blob/master/ruby-align.el

;;; Alignment rules for Ruby
;;; Originals from http://d.hatena.ne.jp/rubikitch/20080227/1204051280

(require 'align)

(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (group 2 3)
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-hash-literal2
               (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Git/Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(git-link magit-filenotify))

(setq magit-status-buffer-switch-function 'switch-to-buffer)

(setq magit-push-always-verify nil)

;; Open magit status in the same window
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

(defun github-pr (&optional prompt)
  (interactive "P")
  (command-execute 'git-link)  ;; this is just here to force autoloading of git-link
  (let* ((remote-name (if prompt (read-string "Remote: " nil nil git-link-default-remote)
                        (git-link--remote)))
         (remote-host (git-link--remote-host remote-name))
         (branch      (git-link--current-branch))
         (commit      (git-link--last-commit))
         (handler     (nth 1 (assoc remote-host git-link-remote-alist))))

    (cond ((null remote-host)
           (message "Unknown remote '%s'" remote-name))
          ((and (null commit) (null branch))
           (message "Not on a branch, and repo does not have commits"))
          ;; functionp???
          ((null handler)
           (message "No handler for %s" remote-host))
          ;; null ret val
          ((browse-url
            (format "https://github.com/%s/compare/%s"
                    (git-link--remote-dir remote-name)
                    branch))))))

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
                           (smartparens-mode -1)
                           (setq web-mode-code-indent-offset 2)   ;; prelude sets this to 4
                           (setq web-mode-markup-indent-offset 2) ;; prelude sets this to 4
                           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Org Drill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-learn)
(require 'org-drill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Smart scan. Use M-n M-p to move forward and backward for symbol at point
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
;;;; Setup: Go Language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(go-mode go-eldoc go-projectile))

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "C-c C-g") 'go-goto-imports)))

(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "C-c C-k") 'godoc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Theme adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (boundp 'zenburn)
    (progn
      (custom-theme-set-variables
       'zenburn
       '(fringe-mode 10 nil (fringe))
       '(linum-format     " %6d "  ))


      (custom-theme-set-faces
       'zenburn
       '(region                              ((t (                       :background "#D0BF8F"))))
       '(highlight                           ((t (                       :background "#D0BF8F"))))
       '(linum                               ((t (:foreground "#ffffff"  :background "#6a6a6a" :height 120 :weight light))))
       '(minibuffer-prompt                   ((t (:foreground "#ffffff"  :background "#F20211"))))
       '(mode-line                           ((t (:foreground "#777777"  :background "#303030" :weight light :box nil :height 125 :inherit (variable-pitch) ))))
       '(fringe                              ((t (                       :background "#4a4a4a"                                               ))))
       )

      )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(flycheck flycheck-pos-tip helm-flycheck))

(defun set-flycheck-defaults()
  (setq flycheck-disabled-checkers '())
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (setq flycheck-idle-change-delay 4))

(add-hook 'flycheck-mode-hook 'set-flycheck-defaults)

(defun toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))


(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)

  (define-key flycheck-mode-map (kbd "C-c C-t") 'toggle-flycheck-error-list)

  ;; Custom fringe indicator
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))

  (let ((bitmap 'my-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-info))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; alias
(eval-after-load "em-alias"
  '(progn (eshell/alias "ll" "ls -la $*")
          (eshell/alias "ff" "find-file $*")
          (eshell/alias "ec" "find-file $*")
          (eshell/alias "g" "git $*")
          (eshell/alias "bi" "bundle install $*")
          (eshell/alias "bil" "bundle install --local $*")
          (eshell/alias "be" "bundle exec $*")
          (eshell/alias "gs" "git status")
          (eshell/alias "gco" "git checkout $*")
          (eshell/alias "rspec" "bundle exec rspec $*")
          (eshell/alias "d" "dired")
          ))

;; Case? It matters not.
(setq eshell-cmpl-ignore-case t)

(setq eshell-prompt-regexp "^[^#$]*[$#] ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Popwin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'popwin)

(require 'popwin)
(popwin-mode)

(setq display-buffer-function 'popwin:display-buffer)

(add-to-list 'popwin:special-display-config '("*rspec-compilation*" :noselect t))
(add-to-list 'popwin:special-display-config '("*Help*" :noselect t))
(add-to-list 'popwin:special-display-config '("*alchemist help*" :noselect t))
(add-to-list 'popwin:special-display-config '("^\\*Flycheck.+\\*$" :regexp t :dedicated t :position bottom :stick t :noselect t))
(delete "*Shell Command Output*" popwin:special-display-config)
(delete "*vc-diff*" popwin:special-display-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Smart parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(smartparens))

(if (boundp 'zenburn)
    ;; Use a more subtle colour for smartparens overlays
    (custom-set-faces
     '(sp-pair-overlay-face ((t (:inherit highlight :background "gray16")))))
  (custom-set-faces
   '(sp-pair-overlay-face nil))
  )

;; Does not seem to be working
;;(setq-default sp-escape-quotes-after-insert nil)
(setq-default sp-escape-wrapped-region nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Erlang/Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(alchemist flycheck-mix ob-elixir))

(flycheck-mix-setup)

(add-hook 'alchemist-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook 'alchemist-mode)

(defconst elixir--prettify-symbols-alist
  '(("fn"  . ?λ)))

(defun custom-elixir-mode-hook ()
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("fn"  . ?λ) prettify-symbols-alist)
  (push '("|>"  . ?ǁ) prettify-symbols-alist)
  (flycheck-mode)
  (smartparens-mode -1)
  (define-key elixir-mode-map (kbd "M-,") 'alchemist-goto-jump-back)

  (define-key ruby-mode-map (kbd "<f5>") (lambda() (interactive) (close-rspec-output-buffer) (rspec-verify)))
  (define-key ruby-mode-map (kbd "M-<f5>") (lambda() (interactive) (close-rspec-output-buffer) (rspec-verify-all)))

  (define-key elixir-mode-map (kbd "<f5>") 'alchemist-mix-test-at-point)
  (define-key elixir-mode-map (kbd "S-<f5>") 'alchemist-mix-test)
  (define-key elixir-mode-map (kbd "C-c , t") 'alchemist-project-toggle-file-and-tests)
  (define-key elixir-mode-map (kbd "C-c , c") (lambda() (interactive) (alchemist-mix-execute (list "compile" nil) nil)))
  )
(add-hook 'elixir-mode-hook 'custom-elixir-mode-hook)

;; Add elixir to hide-show mode
(add-to-list 'hs-special-modes-alist
             '(elixir-mode
               "do" "end" "#"
               nil nil))

;; Save before running tests
(advice-add 'alchemist-mix-test-at-point :before #'save-all)
(advice-add 'alchemist-mix-test :before #'save-all)

(defadvice alchemist-mix-test-at-point (before alchemist-mix-test-at-point-save-all () activate)
  "Save the buffer before runng shell command"
  (simple-save-some-buffers))


;; Add smart-paren support for dealing with do..end blocks to Elixir
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Smartline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sml/theme 'dark)
(require 'smart-mode-line)
(setq sml/hidden-modes "yas\\|Golden\\|GitGutter\\|Projectile\\|RubyRef\\|rt\\|rails\\|,\\||\\|Pre\\|Dim\\|HS\\|wb")
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Auto dim other buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(auto-dim-other-buffers))

;; Customize auto dim other buffers
(setq auto-dim-other-buffers-dim-on-focus-out nil)

(if (equal prelude-theme 'farmhouse-light)
    (custom-set-faces
     '(auto-dim-other-buffers-face ((t (:background "WhiteSmoke")))))
  (custom-set-faces
   '(auto-dim-other-buffers-face ((t (:background "#00001F")))))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup: Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(cucumber-goto-step
                            visual-regexp
                            discover
                            pig-mode
                            nginx-mode
                            idle-highlight-mode
                            ws-butler
                            mkdown
                            ansible
                            emmet-mode
                            htmlize
                            ibuffer-vc
                            jade-mode
                            terraform-mode
                            hideshowvis
                            restclient
                            csv-mode
                            elmacro
                            neotree
                            logstash-conf
                            ))

(setq logstash-indent 2)

(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

(require 'auto-highlight-symbol)

(global-auto-highlight-symbol-mode)

(add-hook 'prog-mode-hook 'ws-butler-mode)

(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(setq adaptive-wrap-extra-indent 4)

(define-key global-map (kbd "M-g M-r") 'vr/replace)
(define-key global-map (kbd "M-g r") 'vr/query-replace)

(global-set-key (kbd "M-g t") 'neotree-toggle)

;; Enable company mode for elisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)

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

