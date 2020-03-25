;; stuff automatically added
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-program-name "aspell")
  '(package-selected-packages
     '(forge magit treemacs-projectile treemacs auto-complete org-jira confluence editorconfig highlight-parentheses counsel-projectile projectile diminish counsel swiper ivy ace-window org-bullets which-key try use-package)))
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
  (load-theme 'misterioso t)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 13"))
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

(use-package magit
  :ensure t
  :diminish
  :bind (("M-g" . 'magit-status)))

(use-package forge
  :ensure t
  :after magit)
  

(use-package projectile
  :ensure t
  :diminish t
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 22)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

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

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char-timer)))

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

(use-package auto-complete
  :ensure t
  :diminish
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

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
(use-package org
  :ensure t
  :config
  (require 'org-tempo))

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
