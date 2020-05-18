;; stuff automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages '(projectile which-key use-package)))
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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

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
  ;; Set buffer list
  (defalias 'list-buffers 'ibuffer-other-window))

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
	  insert-directory-program "/usr/local/bin/gls"
	  dired-listing-switches "-aBhl --group-directories-first")))

(use-package flyspell
  :config
  (setq ispell-list-command "--list"
	ispell-program-name "/usr/local/bin/aspell")
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode)))

(use-package js
  :ensure nil
  :hook (js-mode-hook . js-jsx-enable))

(use-package which-key
  :config (which-key-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))
