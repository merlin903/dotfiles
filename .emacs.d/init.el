;; The default is 800 kibibytes, measured in bytes
(setq gc-cons-threshold (* 500 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Keep .emacs.d clean
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temp file
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

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; Helper function for changing OS platform keywords to system-type strings
(defun platform-keyword-to-string (platform-keyword)
  (cond
   ((eq platform-keyword 'windows) "windows-nt")
   ((eq platform-keyword 'cygwin) "cygwin")
   ((eq platform-keyword 'osx) "darwin")
   ((eq platform-keyword 'linux) "gnu/linux")))

;; Define a macro that runs an elisp expression only on a particular platform
(defmacro on-platform-do (&rest platform-expressions)
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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish t
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-create-definer sa/leader-key-def
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer sa/ctrl-c-keys
    :prefix "C-c"))

(use-package use-package-chords
  :config (key-chord-mode 1))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)          ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)             ; Disable tooltips
(set-fringe-mode 10)          ; Give soem breathing room
(menu-bar-mode -1)            ; Disable the menu bar

(setq visible-bell t)         ; Set up the visible bell

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; One line at a time
      mouse-wheel-progressive-speed nil            ;; Don't accelerate scrolling
      mouse-wheel-follow-mouse 't                  ;; Scroll window under mouse
      scroll-step 1)                               ;; Keyboard scroll one line at a time

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		erc-mode-hook
		term-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		;; neotree-mode-hook
		;; telega-chat-mode-hook
		;; telega-root-mode-hook
		;; telega-webpage-mode-hook
		;; dashboard-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil
      vc-follow-symlinks t
      ad-redefinition-action 'accept)

(use-package doom-themes
  :config
  (load-theme 'doom-solarized-dark))

;; Set the font face based on platform
(on-platform-do
 (osx (set-face-attribute 'default nil :font "JetBrains Mono" :height 135)))

(defun sa/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
		     (lambda (i) (string-equal (car i) block-name))
		     unicode-fonts-block-font-mapping))
	 (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
	 (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
	  `(,updated-block))))

(use-package unicode-fonts
  :ensure t
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
   (lambda (block-name)
     (sa/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package diminish)

;; You must run (all-the-icons-install-fints) one time after
;; installing this package.

(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minons-mode-line-lighter ""))

(use-package doom-modeline
  ;; :after eshell     ;; Make sure it gets hooked after eshell
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 1.10))))
  (mode-line-inactive ((t (:height 1.10)))))

(use-package alert
  :commands alert
  :config (setq alert-default-style 'growl
		alert-log-messages t))

(defun sa/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (visual-line-mode 0)
  (diminish org-indent-mode))

(defun sa/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.0)
		  (org-level-6 . 1.0)
		  (org-level-7 . 1.0)
		  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Helvetica" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be bixed-pitch in Org files appears that way
  (set-face-attribute 'org-block           nil :foreground nil  :inherit                         'fixed-pitch)
  (set-face-attribute 'org-code            nil                  :inherit '(shadow                 fixed-pitch))
  (set-face-attribute 'org-table           nil                  :inherit '(shadow                 fixed-pitch))
  (set-face-attribute 'org-verbatim        nil                  :inherit '(shadow                 fixed-pitch))
  (set-face-attribute 'org-special-keyword nil                  :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line       nil                  :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox        nil                  :inherit                         'fixed-pitch))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :hook (org-mode . sa/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
	org-directory "~/Documents/OrgFiles")

  (sa/org-font-setup)

  ;; NOTE: Subsequent sections are still part of this use-package block!

(defun sa/org-path (path)
  (expand-file-name path org-directory))

(setq org-default-notes-file (sa/org-path "Notes.org")
      org-agenda-files (list org-directory))

(setq org-agenda-window-setup 'other-window
      org-agenda-span 'day
      org-stuck-projects '("+LEVEL=2/TODO" ("NEXT") nil "")
      org-agenda-start-with-log-mode t)

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "PROC" ((org-agenda-overriding-header "Process Tasks")))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))

	("p" "Active Projects"
	 ((agenda "")
	  (todo "ACTIVE"
		((org-agenda-overriding-header "Active Projects")
		 (org-agenda-max-todos 5)
		 (org-agenda-files org-agenda-files)))))

	("w" "Workflow Status"
	 ((todo "WAIT"
		((org-agenda-overriding-header "Waiting on External")
		 (org-agenda-files org-agenda-files)))
	  (todo "REVIEW"
		((org-agenda-overriding-header "In Review")
		 (org-agenda-files org-agenda-files)))
	  (todo "PLAN"
		((org-agenda-overriding-header "In Planning")
		 (org-agenda-files org-agenda-files)))
	  (todo "BACKLOG"
		((org-agenda-overriding-header "Project Backlog")
		 (org-agenda-files org-agenda-files)))
	  (todo "READY"
		((org-agenda-overriding-header "Ready for Work")
		 (org-agenda-files org-agenda-files)))
	  (todo "ACTIVE"
		((org-agenda-overriding-header "Active Projects")
		 (org-agenda-files org-agenda-files)))
	  (todo "COMPLETED"
		((org-agenda-overriding-header "Completed Projects")
		 (org-agenda-files org-agenda-files)))
	  (todo "CANCELLED"
		((org-agenda-overriding-header "Canclled Projects")
		 (org-agenda-files org-agenda-files)))))

	;; Projects on hold
	("h" tags-todo "+LEVEL=2/+HOLD"
	 ((org-agenda-overriding-header "On-hold Projects")
	  (org-agenda-files org-agenda-files)))

	;; Low-effort next actions
	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))))

;; Configure common tags
(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
	(:endgroup)
	("@errand" . ?E)
	("@home" . ?H)
	("@work" . ?W)
	("agenda" . ?a)
	("planning" . ?p)
	("publish" . ?P)
	("batch" . ?b)
	("note" . ?n)
	("idea" . ?i)
	("thinking" . ?t)
	("recurring" . ?r)))

;; Configure TODO settings
(setq org-log-done 'time
      org-log-into-drawer t
      org-datetree-add-timestamp 'inactive
      org-fontify-whole-heading-line t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROC" "|" "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(k@)")
	(sequence "GOAL(g)" "|" "ACHIEVED(v)" "MAINTAIN(m)")))

)

(defun sa/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'sa/org-babel-tangle-dont-ask
					      'run-at-end 'only-in-org-mode)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
