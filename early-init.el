;; Disable these as they are used seldom
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Show full caret position in the mode bar
(line-number-mode t)
(column-number-mode t)

;; When line numbers are display, pre allocate space for them.
;; at least 3 digits. So that the line number column grow and
;; shrink unneccassarily.
(setq-default display-line-numbers-width 3)

;; disable native compilation errors, as they are not relevant
;; to me, at least with my current usage.
(setq native-comp-async-report-warnings-errors 'silent)

;; I like a tidy .emacs.d dir. So keep the eln-cache
;; in a separate dir, out of my vicinity.
(defvar ndb-native-comp-cache-dir
  (convert-standard-filename
   (expand-file-name "var/eln-cache" user-emacs-directory))
  "Directory for Emacs native compilation caches.")
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  ndb-native-comp-cache-dir))

;; Initialise intalled packages early. As explained by Protsilaos
;; in his early-init.el
(setq package-enable-at-startup t)

;; Load the newest bytecode found.
(setq load-prefer-newer t)

;; I will open large files, and it is okay.
(setq large-file-warning-threshold (* 100 1024 1024))

;; Do not create backup files and lockfiles
(setq make-backup-files nil
      create-lockfiles nil)

;; Another startup speed up by Protesilaos. The idea is to disable
;; GC and VC handling at startup, and then restore them after
;; Emacs is fully started up.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)
(setq vc-handled-backends nil)

;; Restore above after startup.
(defun ndb-restore-early-inhibitions ()
  "This will restore any inhibitions set at early initialization."
  (setq gc-cons-threshold (* 50 1024 1024)
	gc-cons-percentage 0.1
	vc-handled-backends prot-emacs--vc-handled-backends))
(add-hook 'emacs-startup-hook #'ndb-restore-early-inhibitions)

;; Set the frame name to indicate the completion of initialization
(add-hook 'after-init-hook (lambda () (set-frame-name "NDB Emacs")))


