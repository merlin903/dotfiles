(defun sha/setup-projectile ()
  "Install and configure Projectile"
  (use-package projectile
    :commands projectile-mode projectile-project-name
    :init
    (add-hook 'after-init-hook 'projectile-mode)
    :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq frame-title-format '((:eval (projectile-project-name))))
    ))
