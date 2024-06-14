;;; plan37-lang-c.el --- Configurations for C        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nisal D. Bandara

;; Author: Nisal D. Bandara <nisalb@hey.com>
;; Keywords: convenience, c

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures Emacs for better C programming experience.

;;; Code:

(use-package cc-mode
  :ensure nil
  :config
  ;; A new c style for PostgreSQL source files
  (c-add-style "postgresql"
               '("bsd"
                 (c-auto-align-backslashes . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist . ((case-label . +)
                                     (label . -)
                                     (statement-case-open . +)))
                 (fill-column . 78)
                 (indent-tabs-mode . t)
                 (tab-width . 4)))

  (defvar plan37--use-postgresql-style-regex "/postgre\\(ql\\)?/"
    "In C-mode, if the opened `buffer-file-name' matches this string, its
C style is set to `postgresql' style.")
  (defun plan37--use-postgresql-style-if-possible ()
    "Enable `postgresql' style if applicable to the opened buffer."
    (when (string-match plan37--use-postgresql-style-regex buffer-file-name)
      (c-set-style "postgresql")
      ;; This might not be required. But kept for portability.
      (set (make-local-variable 'ignored-local-variables)
           (append '(c-file-style) ignored-local-variables))))
  (add-hook 'c-mode-hook #'plan37--use-postgresql-style-if-possible)

  ;; default C styles
  (setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "bsd")))
  (setq c-basic-offset 4)

  ;; documentation style
  (setq c-doc-comment-style '((java-mode . javadoc) (c-mode . doxygen)))

  ;; make RET to indent the new line
  (defun plan37--make-CR-do-indent ()
    (define-key c-mode-base-map (kbd "<ret>") 'c-context-line-break))
  (add-hook 'c-initialization-hook #'plan37--make-CR-do-indent))

(use-package meson-mode
  :ensure t
  :init
  (setq meson-indent-basic 4))

(provide 'plan37-lang-c)
;;; plan37-lang-c.el ends here
