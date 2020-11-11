;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(if (file-exists-p "~/.emacs.d/per-system-settings.el")
    (load-file "~/.emacs.d/per-system-settings.el"))

(defvar sa/is-ish nil
  "Determine if current system is iSH shell on an iPhone.")
(defvar sa/is-darwin nil
  "Determine if current system is Darwin based.")

(require 'subr-x)
(setq sa/is-ish
      (string-match-p (regexp-quote "iSH") (string-trim (shell-command-to-string "uname -a"))))

(setq sa/is-darwin
      (string-match-p (regexp-quote "Darwin") (string-trim (shell-command-to-string "uname -a"))))

;; Keep transient cruft out of ~/.emacs.d
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Add advice to automatically refresh packages
(defvar sa/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")

(defun sa/package-refresh (&rest args)
  "Refresh package metadata, if needed.
Ignores `ARGS'."
  (unless (eq sa/packages-refreshed t)
    (progn
      (package-refresh-contents)
      (setq sa/packages-refreshed t))))

(advice-add 'package-install :before #'sa/package-refresh)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Ensure packages by default
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

;; Add my elisp path to load-path
(push "~/.emacs.d/elisp" load-path)

(defun platform-keyword-to-string (platform-keyword)
  "Helper function for changing OS platform keywords to system-type strings"
  (cond
   ((eq platform-keyword 'widnows) "windows-nt")
   ((eq platform-keyword 'cygwin) "cygwin")
   ((eq platform-keyword 'osx) "darwin")
   ((eq platform-keyword 'linux) "gnu/linux")))

(defmacro on-platform-do (&rest platform-expressions)
  "Runs an elisp expression only on a particular platform"
  `(cond
    ,@(mapcar
       (lambda (platform-expr)
	 (let ((keyword (nth 0 platform-expr))
	       (expr (nth 1 platform-expr)))
	   `(,(if (listp keyword)
		  `(or
		    ,@(mapcar
		       (lambda (kw) `(string-equal system-type ,(platform-keyword-to-string kw)))
		       keyword))
		`(string-equal system-type ,(platform-keyword-to-string keyword)))
	     ,expr)))
       platform-expressions)))

(server-start)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-create-definer sa/leader-key-def
    :global-prefix "C-M-SPC")
  (general-create-definer sa/ctrl-c-keys
    :prefix "C-c"))

;; Disable the startup message
(setq inhibit-startup-message t)

(unless sa/is-ish
  (scroll-bar-mode -1)			; Disable visible scrollbar
  (tool-bar-mode -1)			; Disable the toolbar
  (tooltip-mode -1)			; Disable tooltips
  (set-fringe-mode 10))			; Give some breathing room

(menu-bar-mode -1)			; Disable the menu bar

(setq visible-bell t)			; Set up the visible bell

(unless sa/is-ish
  (setq mouse-wheel-scroll-amount
	'(1 ((shift) . 1)))		; One line at a time
  (setq mouse-wheel-progressive-speed
	nil)				; Don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't)	; Scroll window under mouse
  (setq scroll-step 1))			; Keyboard scroll one line at a time

(unless sa/is-ish
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package doom-themes
  :config
  (unless sa/is-ish
    (load-theme 'doom-solarized-dark)
    (doom-themes-visual-bell-config))
  (unless sa/is-darwin
    (load-theme 'misterioso)))

(unless sa/is-ish
  (on-platform-do
   ((windows cygwin) (set-face-attribute 'default nil :font "JetBrains Mono:antialias=subpixel" :height 130))
   (osx (set-face-attribute 'default nil :font "JetBrains Mono" :height 140))
   (linux (set-face-attribute 'default nil :font "JetBrains Mono" :height 220))))

;; (defun sa/replace-unicode-font-mapping (block-name old-font new-font)
;;   (let* ((block-idx (cl-position-if
;; 		     (lambda (i) (string-equal (car i) block-name))
;; 		     unicode-fonts-block-font-mapping))
;; 	 (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
;; 	 (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
;;     (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
;; 	  `(,updated-block))))

;; (use-package unicode-fonts
;;   :if (not sa/is-ish)
;;   :custom
;;   (unicode-fonts-skip-fonts-groups '(low-quality-glyphs))
;;   :config
;;   ;; Fix the font mappings to use the right emoji font
;;   (mapcar
;;    (lambda (block-name)
;;      (sa/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
;;    '("Dingbats"
;;      "Emoticons"
;;      "Miscellaneous Symbols and Pictorgraphs"
;;      "Transport and Map Symbols"))
;;   (unicode-fonts-setup))

;; (use-package emojify
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish)
