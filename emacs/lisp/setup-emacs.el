(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun sha/setup-emacs ()
  "Configure emacs UI and variables"
  (use-package emacs
    :init
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    (global-prettify-symbols-mode)
    (global-linum-mode t)
    (set-frame-font "JetBrains Mono-13" nil t)
    )
  (use-package solarized-theme
    :config
    (load-theme 'solarized-dark t)
    (transparency 100)
    )
  )
