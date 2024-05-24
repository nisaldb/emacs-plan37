;;; plan37-editor.el --- Editing features            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nisal D. Bandara

;; Author: Nisal D. Bandara <nisalb@hey.com>
;; Keywords: convenience

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

;; This adds plugins and configurations for editing features

;;; Code:

(use-package vundo
  :ensure t
  :defer 1
  :bind (:map global-map
	      ("C-c u" . vundo)
	 :map vundo-mode-map
	      ("C-/" . vundo-backward)
	      ("C-?" . vundo-forward)
	      ("u" . vundo-backward)
	      ("U" . vundo-forward)
	      ("g" . vundo-goto-last-saved)
	      ("." . vundo-goto-last-saved)
	      ("h" . vundo-backward)
	      ("j" . vundo-next)
	      ("k" . vundo-previous)
	      ("l" . vundo-forward))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
	vundo-compact-display t)

  ;; A more clever way to invoke vundo:
  ;; Only if I'm undoing repeatedly.
  (defvar plan37--vundo-repeatable-undo-functions '(undo undo-only undo-redo)
    "If one of these commands are invoked repeatedly, vundo will kick in.")

  (defun plan37--vundo-if-repeat-undo (&rest args)
    "Use `vundo' if the last command is in `plan37--vundo-repeatable-undo-functions'.
That is if I'm using undo more than once, start visualizing the undo tree."
    (interactive)
    (if (and (member last-command plan37-vundo-if-repeat-undo)
	     (not undo-in-region))
	(call-interactively 'vundo)
      (apply args)))

  (mapc
   (lambda (fn)
     (advice-add fn :around #'plan37--vundo-if-repeat-undo))
   plan37--vundo-repeatable-undo-functions))
(provide 'plan37-editor)
;;; plan37-editor.el ends here
