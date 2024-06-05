;;; plan37-lang-ruby.el --- Ruby configurations      -*- lexical-binding: t; -*-

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

;; This file installs and configures Ruby programming

;;; Code:

(add-to-list 'completion-ignored-extensions ".rbc")

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package yari
  :ensure t
  :bind ("C-h R" . yari))

(use-package ruby-mode
  :ensure nil
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (subword-mode +1))

(provide 'plan37-lang-ruby)
;;; plan37-lang-ruby.el ends here
