;;; plan37-programming.el --- Generic programming enhancements  -*- lexical-binding: t; -*-

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

;; This enhance the general programming experience in Emacs.
;; These configurations may relate to a specific class of programming
;; languages such as Lisps or C-mode languages, but not for a specific
;; programming language.

;;; Code:

;; Syntax checking with Flycheck.
;; I find FlyMake to be hard to configure
;; TODO:: can I use FlyMake instead?
(use-package flycheck
  :ensure t
  ;; List of Flycheck enabled major modes
  ;; TODO:: move this into a custom variable
  :hook (emacs-lisp-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(provide 'plan37-programming)
;;; plan37-programming.el ends here
