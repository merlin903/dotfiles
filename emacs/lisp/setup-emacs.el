(defun sha/configure-emacs ()
  "Configure Emacs core settings"
  (use-package emacs
    :init
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    ))
