;;; plan37-ui.el --- Aesthetic modifications to Emacs  -*- lexical-binding: t; -*-

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

;; This includes configurations related to any aesthetic aspect
;; of my Emacs configuration.

;;; Code:

;; Hide vertical scroll bars
(push '(vertical-scroll-bars . 0) default-frame-alist)

;; Hide the right fringe
(push '(right-fringe . 0) default-frame-alist)

;; Start Emacs maximized
(push '(fullscreen . maximized) default-frame-alist)

;; Smoother scrolling
(add-hook 'after-init-hook 'pixel-scroll-precision-mode)

;; Diminish for mode line
(use-package diminish
  :ensure t)

;; Color palette.
(use-package ef-themes
  :ensure t
  :demand t
  :config
  (setq ef-themes-to-toggle '(ef-duo-light
			      ef-dream))
  (progn
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'ef-duo-light :noconfirm))
  (global-set-key (kbd "C-c T") '("Toggle theme" . ef-themes-toggle)))

;; Font
(use-package fontaine
  :ensure t
  :demand t
  :hook
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
		   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :config
  (setq fontaine-latest-state-file (expand-file-name "var/fontaine-latest-state.eld" user-emacs-directory))
  (setq fontaine-presets
	'((regular
	   :default-family "Iosevka Comfy Motion"
	   :default-height (if (eq system-type 'darwin) 130 100))
	  (t
	   :default-family "Iosevka Comfy Motion"
	   :default-height (if (eq system-type 'darwin) 130 100)
	   :default-slant normal
	   :default-weight regular

	   :fixed-pitch-family "Iosevka Comfy Motion"
	   :fixed-pitch-height 1.0
	   :fixed-pitch-weight nil
	   :fixed-pitch-slant nil

	   :fixed-pitch-serif-family nil
	   :fixed-pitch-serif-height 1.0
	   :fixed-pitch-serif-weight nil
	   :fixed-pitch-serif-slant nil

	   :variable-pitch-family "Iosevka Comfy Motion Duo"
	   :variable-pitch-height 1.0
	   :variable-pitch-weight nil
	   :variable-pitch-height nil

	   :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-weight bold
           :bold-slant nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-height 1.0

           :line-spacing nil))))

;; Dashboard at the startup
(use-package dashboard
  :ensure t
  :demand t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Acta, non Verba"
	dashboard-center-content t
	dashboard-footer-messages `(,(concat "Configured by " user-full-name "."))
	dashboard-items '((recents . 5)
			  (projects . 5)
			  (bookmarks . 5)))
  (dashboard-setup-startup-hook))

;; Keybinding hints
(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Switch windows more easily
(use-package ace-window
  :ensure t
  :demand t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?z ?x ?c ?v)
	aw-scope 'frame))

;; Add some padding to the buffer.
;; This is currently on-demand by <f8>
;; (use-package spacious-padding
;;   :ensure t
;;   :if (display-graphic-p)
;;   :defer
;;   :bind ("<f8>" . spacious-padding-mode)
;;   :init
;;   (setq spacious-padding-subtle-mode-line
;; 	`( :mode-line-active help-key-binding
;; 	   :mode-line-inactive window-divider)))

(provide 'plan37-ui)
;;; plan37-ui.el ends here
