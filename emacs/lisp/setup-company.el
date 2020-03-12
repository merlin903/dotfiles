(defun sha/setup-company-mode ()
  "Install and configure company-mode"
  (use-package company
    :init
    (global-company-mode)
    :bind
    (("M-<tab>" . company-begin-commands))
    :config
    (setq company-transformers '(company-sort-by-occurrence)
	  company-tooltip-limit 30
	  company-idle-delay 2
	  company-echo-delay 0)
    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    )
  (use-package company-box
    :disabled
    :after (company)
    :hook (company-mode . company-box-mode))
)
