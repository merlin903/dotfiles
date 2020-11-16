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

;; Add /usr/local/bin to exec-path
(push "/usr/local/bin" exec-path)

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

(use-package try)

(server-start)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-create-definer sa/leader-key-def
    :prefix "C-SPC")
  (general-create-definer sa/ctrl-c-keys
    :prefix "C-c"))

;; Disable the startup message
(setq inhibit-startup-message t)

(unless sa/is-ish
  (scroll-bar-mode -1)		; Disable visible scrollbar
  (tool-bar-mode -1)			; Disable the toolbar
  (tooltip-mode -1)			; Disable tooltips
  (set-fringe-mode 10))		; Give some breathing room

(menu-bar-mode -1)			; Disable the menu bar

(setq visible-bell t)			; Set up the visible bell

(unless sa/is-ish
  (setq mouse-wheel-scroll-amount
	'(1 ((shift) . 1)))		; One line at a time
  (setq mouse-wheel-progressive-speed
	nil)				; Don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't)	; Scroll window under mouse
  (setq scroll-step 1))		; Keyboard scroll one line at a time

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

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))

(use-package alert
  :commands alert
  :ensure-system-package growlnotify
  :config
  (setq alert-default-style 'growl))

(global-auto-revert-mode 1)

(sa/leader-key-def
  "t" '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package paren
  :config (show-paren-mode 1))

(setq display-time-world-list
      '(("America/Los_Angeles" "Los Angeles")
	("America/Mexico_City" "Mexico City")
	("America/New_York" "New York")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

(use-package pinentry
  :ensure-system-package gpg
  :custom
  (epg-pinentry-mode 'loopback)
  :init
  (pinentry-start))

(setq tramp-default-method "ssh")

(use-package editorconfig
  :ensure-system-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package parinfer
  :hook ((clojure-mode . parinfer-mode)
	 (emacs-lisp-mode . parinfer-mode)
	 (common-lisp-mode . parinfer-mode)
	 (scheme-mode . parinfer-mode)
	 (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
	'(defaults      ; should be included
	   pretty-parens    ; different paren styles for different modes
	   smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab
	   smart-yank)))    ; Yank behavior depends on mode

(sa/leader-key-def
  "tp" 'parinfer-toggle-mode)

(defun sa/org-file-jump-to-heading (org-file heading-title)
  "Jump to a specific heading in an Org file"
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun sa/org-file-show-headings (org-file)
  "Show headings in an Org file"
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(sa/leader-key-def
  "f" '(:ignore t :which-key "files")
  "fb" '((lambda () (interactive) (counsel-find-file "~/Documents/OrgFiles/")) :which-key "beorg")
  "fd" '(:ignore t :which-key "dotfiles")
  "fdd" '((lambda () (interactive) (find-file "~/Development/sametjan/dotfiles/Desktop.org")) :which-key "desktop")
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/Development/sametjan/dotfiles/Emacs.org"))) :which-key "edit config")
  "fdE" '((lambda () (interactive) (sa/org-file-show-headings "~/Development/sametjan/dotfiles/Emacs.org")) :which-key "edit config")
  "fds" '((lambda () (interactive) (sa/org-file-jump-to-heading "~/Development/sametjan/Systems.org" "Base Configuration")) :which-key "base system")
  "fdS" '((lambda () (interactive) (sa/org-file-jump-to-heading "~/Development/sametjan/Systems.org" system-name)) :which-key "this system"))

(use-package hydra
  :defer 1)

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         ("C-k" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         ("C-k" . ivy-reverse-i-search-kill))
  :init (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-buffers t)
  :config
  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package smex      ;Adds M-x recent commands sorting for counsel-M-x
  :defer 1
  :after counsel)

(use-package wgrep)

(use-package ivy-posframe
  :custom
  (ivy-posframe-width 115)
  (ivy-posframe-win-width 115)
  (ivy-posframe-height 10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-parameters '((parent-frame . nil)
  ;;                                 (left-fringe . 8)
  ;;                                 (right-frindge . 8)))
  (ivy-posframe-mode 1))

(sa/leader-key-def
  "r" '(ivy-resume :which-key "ivy resume")
  "ff" '(counsel-find-file :which-key "open file")
  "C-f" 'counsel-find-file
  "fr" '(counsel-recentf :which-key "recent files")
  "fR" '(revert-buffer :which-key "revert file")
  "fj" '(counsel-file-jump :which-key "jump to file"))

(use-package xwidget
  :custom
  (browse-url-browser-function 'xwidget-webkit-browse-url))

(use-package search-web
  :bind ("C-c w" . 'search-web)
  :config
  (defun browse-url-default-browser (url &rest args)
    "Override `browse-url-default-browser' to use `xwidget-webkit' URL ARGS."
    (xwidget-webkit-browse-url url args)))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config (global-set-key (kbd "C-M-;") 'magit-status))

(sa/leader-key-def
  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gd" 'magit-diff-unstaged
  "gc" 'magit-branch-or-checkout
  "gl" '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb" 'magit-branch
  "gP" 'magit-push-current
  "gp" 'magit-pull-branch
  "gf" 'magit-fetch
  "gF" 'magit-fetch-all
  "gr" 'magit-rebase)

(use-package forge
  :after magit)

(use-package eglot)

(use-package jedi
  :ensure-system-package (jedi . "pipx install jedi-language-server")
  :hook (python-mode . jedi:setup))
