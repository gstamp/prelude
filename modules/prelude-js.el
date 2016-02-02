;;; prelude-js.el --- Emacs Prelude: js-mode configuration.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for js-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'prelude-programming)
(prelude-require-packages '(js3-mode json-mode))

(require 'js3-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js3-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js3-mode))
(add-to-list 'interpreter-mode-alist '("node" . js3-mode))

;; (eval-after-load 'js2-mode
;;   '(progn
;;      (defun prelude-js-mode-defaults ()
;;        ;; electric-layout-mode doesn't play nice with smartparens
;;        (setq-local electric-layout-rules '((?\; . after)))
;;        (setq mode-name "JS2")
;;        (js2-imenu-extras-mode +1))

;;      (setq prelude-js-mode-hook 'prelude-js-mode-defaults)

;;      (add-hook 'js2-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))))

(eval-after-load 'js3-mode
  '(progn
     (defun prelude-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after)))
       (setq mode-name "JS3")
       (setq js3-auto-indent-p t)
       (setq js3-enter-indents-newline t)
       (setq js3-indent-on-enter-key t)
       )

     (setq prelude-js-mode-hook 'prelude-js-mode-defaults)

     (add-hook 'js3-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))))


(provide 'prelude-js)

;;; prelude-js.el ends here
