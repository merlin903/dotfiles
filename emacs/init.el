;; stuff automatically added
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-program-name "aspell")
 '(package-selected-packages
   '(org-jira confluence editorconfig highlight-parentheses counsel-projectile projectile diminish counsel swiper ivy ace-window org-bullets which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; Our stuff
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")
			                    ("org" . "https://orgmode.org/elpa")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(use-package emacs
  :config
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode 1)
  (load-theme 'misterioso)
  (set-frame-font "JetBrains Mono 13" nil t)
  (setq make-backup-file -1)
  (defalias 'list-buffers 'ibuffer-other-window))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package js
  :hook (js-mode-hook . js-jsx-enable))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode 1))
  

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("M-o" . 'ace-window))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))

(use-package dired
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
	  insert-directory-program "/usr/local/bin/gls"
	  dired-listing-switches "-aBhl --group-directories-first")))

(use-package editorconfig
  :ensure t
  :diminish 
  :config
  (editorconfig-mode 1))

(use-package confluence
  :ensure t
  :diminish
  :init
  (setq confluence-url "https://wiki.hulu.com/rpc/xmlrpc")
  :bind (("C-x w f" . confluence-get-page))
  :config (add-hook 'confluence-edit-mode-hook
		    (local-set-key "\C-xw" confluence-prefix-map)
		    (local-set-key "\M-j" 'confluence-newline-and-indent)
		    (local-set-key "\M-;" 'confluence-list-indent-dwim)))

;; Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))) 

(use-package org-jira
  :ensure t
  :init
  (setq jiralib-url "https://jira.hulu.com"
    org-jira-working-dir "~/Documents/Org/Jira"
    org-jira-project-filename-alist (quote (("XPM" . "XPM.org"))))
  :bind (("C-x j g i" . org-jira-get-issues)
	 ("C-x j g s" . org-jira-get-subtasks)))