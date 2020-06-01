;; STUFF automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages
     '(counsel-tramp editorconfig try pyenv-mode virtualenv elpy magit counsel swiper ivy which-key use-package projectile forge diminish)))
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
			                    ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(defvar sha/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")
(defun sha/package-refresh (&rest args)
  "Refresh package metadata, if needed.
Ignores `ARGS'."
  (unless (eq sha/packages-refreshed t)
    (progn
      (package-refresh-contents)
      (setq sha/packages-refreshed t))))
(advice-add 'package-install :before #'sha/package-refresh)

(defun sha/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
    (prin1 (eval (read (current-kill 0)))
      (current-buffer))
    (error (message "Invalid expression")
      (insert (current-kill 0)))))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)
(use-package try)

(use-package emacs
  :ensure nil
  :config
  ;; Backup Files
  (setq backup-directory-alist `(("." . "~/.saves"))
	  backup-by-copying t
	  delete-old-versions t
	  kept-new-versions 5
	  kept-old-versions 2
	  version-control t)
  ;; Disable annoyances
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Set Theme and Font
  (load-theme 'misterioso)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 13"))
  ;; Enable UI features
  (global-linum-mode 1)
  (delete-selection-mode 1)
  ;; Set buffer list
  (defalias 'list-buffers 'ibuffer-other-window)
  (global-set-key (kbd "C-c e") 'sha/eval-and-replace))

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
	    insert-directory-program "/usr/local/bin/gls"
	    dired-listing-switches "-aBhl --group-directories-first")))

(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq ispell-list-command "--list"
	  ispell-program-name "/usr/local/bin/aspell")
  :hook ((prog-mode . flyspell-prog-mode)
	        (text-mode . flyspell-mode)))

(use-package js
  :ensure nil
  :hook (js-mode-hook . js-jsx-enable))

(use-package whitespace
  :init
  (setq whitespace-display-mappings
	  '((newline-mark 10 [172 10])))
  :config
  (global-whitespace-newline-mode 1))

(use-package pyenv-mode
  :init
  (pyenv-mode))
(use-package elpy
  :init
  (setq elpy-rpc-virtualenv-path 'current)
  (elpy-enable))

(use-package which-key
  :diminish
  :config (which-key-mode))

(use-package projectile
  :defer t
  :diminish
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :diminish auto-revert-mode)
(use-package forge
  :after magit
  :diminish)

(use-package ivy
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
	        ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	  ivy-display-style 'fancy
	  ivy-count-format "(%d/%d) "))
(use-package swiper
  :bind ("C-s" . swiper))
(use-package counsel
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
(use-package counsel-tramp
  :bind (("C-c s" . counsel-tramp)))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))
