;;; plan37-completion.el --- Completion UI enhancements  -*- lexical-binding: t; -*-

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

;; These configurations are to enhance the completion UI in Emacs.
;; It will utilize vertico and related third-party packages for this purpose,

;;; Code:

(use-package vertico
  :ensure t
  :demand t
  :hook (after-init . vertico-mode)
  :init
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
	enable-recursive-minibuffers t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq completion-styles '(basic substring initials flex orderless)
	resize-mini-window t)

  ;; Reset all the per-category defaults, so that
  ;;  (1) I can specify my own styles for required categories
  ;;  (2) for unspecified, Emacs will use the `completion-styles'
  (setq completion-category-defaults nil)

  ;; Custom category overrides:
  (setq completion-category-overrides
	'((file (styles . (basic partial-completion orderless)))
	  (buffer (styles . (basic substring partial-completion orderless)))
	  (bookmark (styles . (basic substring)))
	  (embark-keybindings (styles . (basic substring)))
	  (imenu (styles . (basic substring orderless)))
	  (consult-location (styles . (basic substring orderless)))
	  (kill-ring (styles . (emacs22 orderless)))
	  (eglot (styles . (emacs22 substring orderless))))))

(use-package orderless
  :ensure t
  :demand t
  :after minibuffer
  :bind (:map minibuffer-local-completion-map
	      ;; use space for orderless
	      ("SPC" . nil)
	      ;; ? is a regexp construct
	      ("?" . nil))
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))

  ;; Style dispatchers. Inspired (or copied) from Protesilaos
  (defun plan37--orderless-literal-dispatcher (word _index _total)
    "Read WORD= as a literal string."
    (when (string-suffix-p "=" word)
      `(orderless-literal . ,(substring word 0 -1))))

  (defun plan37--orderless-extension-dispatcher (word _index _total)
    "Expand WORD. to a file suffix when completing file names."
    (when (and minibuffer-completing-file-name
	       (string-suffix-p "." word))
      `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

  (defun plan37--orderless-affix-dispatcher (word _index _total)
    "Expand WORD~ to \\(^WORD\\|WORD$\\)."
    (when-let (((string-suffix-p "~" word))
	       (word (substring word 0 -1)))
      `(orderless-regexp . ,(format "\\(^%s\\|%s$\\)" word word))))

  (setq orderless-style-dispatchers
	'(plan37--orderless-literal-dispatcher
	  plan37--orderless-extension-dispatcher
	  plan37--orderless-affix-dispatcher)))

;; Indicate minibuffer depth
(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-window nil))

;; Save minibuffer and related histories
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-save-minibuffer-history t
	history-delete-duplicate t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; Corfu for in-buffer completion
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-cycle t
	corfu-auto nil
	corfu-min-width 20)
  (corfu-popupinfo-mode 1)

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; For annotations in completions
(use-package marginalia
  :ensure t
  :defer 1
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;; Consult is useful for advance searching
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap switch-to-buffer] . 'consult-buffer)
	 ([remap switch-to-buffer-other-window] . 'consult-buffer-other-window)
	 ([remap bookmark-jump] . 'consult-bookmark)
	 ([remap yank-pop] . 'consult-yank-pop)
	 ([remap goto-line] . 'consult-goto-line)
	 ("C-c C-l" . consult-line)
	 ("C-c C-f" . consult-find)
	 ("C-c C-r" . consult-ripgrep))
  :init
  (setq xref-show-xref-function #'consult-xref
	xref-show-definition-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"
	consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )")))

;; TODO:: Is embark a worthy plugin?
  
(provide 'plan37-completion)
;;; plan37-completion.el ends here
