(defun sha/restart-emacs ()
  "Install restart-emacs"
  (use-package restart-emacs
    :bind ("C-c q r" . restart-emacs)
    ))
