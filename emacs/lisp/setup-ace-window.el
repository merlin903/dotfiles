(defun sha/setup-ace-window ()
  "Install and configure ace-window"
  (use-package ace-window
    :bind (("M-o" . 'ace-window))
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    )
  )
