;;; plan37-projects.el --- Plugins and configurations for project mamangement.  -*- lexical-binding: t; -*-

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

;; This installs the necessary plugins for project management and version
;; control purposes.

;;; Code:

(use-package projectile
  :ensure t
  :defer 1
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package project
  :ensure nil
  :bind (("C-x p <return>" . project-dired)
	 ("C-x p C-g" . keyboard-quit))
  :config
  (setopt project-switch-commands
	  '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project" ".projectile")))

;; TODO:: Experiment with VC to find out configurables
(use-package vc
  :ensure nil
  :config
  ;; I'm only using git
  (setq vc-handled-backends '(Git)))

;; Magit is still good for Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq git-commit-style-convention-checks '(non-empty-second-line)))
	
(provide 'plan37-projects)
;;; plan37-projects.el ends here
