;; User information
(setq user-full-name "Nisal D. Bandara"
      user-mail-address "nisalb@hey.com")

;; Add NEXT files to the load path
(defvar plan37-load-paths
  '("core" "langs")
  "A list of directory names relative the `user-emacs-directory',
to be added to the load path.
NOTE Directories are not added recursively.")

(dolist (dir plan37-load-paths load-path)
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

;; Higlight the current line in package list
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; MELPA as a package archive. I'm setting the package-archives
;; variable here since I did not require'd package feature in this file
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Set the priorities for package archives. Higher the number, higher the
;; priority. Unspecified will have 0, so they will be searched last.
(setq package-archive-priorities
      '(("gnu" . 100)
	("nongnu" . 80)
	("melpa" . 70)))

;; Open the last view position of a file
(save-place-mode +1)

;; Replace the selected text with typed text.
;; This is more natural.
(delete-selection-mode +1)

;; Use shorter Y/N type prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; For macOS
(when (eq system-type 'darwin)
  (progn
    (setq mac-option-modifier 'super
	  mac-command-modifier 'meta)))

;; Emacs configuration vars
(use-package emacs
  :ensure nil
  :config
  ;; See plan37-completion.el for Corfu usage.
  (setq tab-always-indent 'complete))

;; Organize the .emacs directory
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (and custom-file
	     (file-exists-p custom-file))
    (load custom-file :nomessage)))

;; Load modules
(require 'plan37-ui)
(require 'plan37-completion)
(require 'plan37-projects)
(require 'plan37-editor)

