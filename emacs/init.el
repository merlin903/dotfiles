;; Emacs init.el -- -*- lexical-binding: t; -*- ;;

(load-file (concat
	    (getenv "HOME")
	    "/Development/hrs/sensible-defaults.el"
	    "/sensible-defaults.el"))
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; Create custom.el file to store any settings changed by using customization interface
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Let's not exit emacs. Deleting the frame is almost always what I want.
;; This is because I run emacs in server mode all the time.
(global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "C-x M-c") 'kill-emacs)

(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory (concat user-emacs-directory "lisp"))

;; Functions loaded from lisp directory
(sha/who-am-i "Steven Ametjan" "steve@hittingthebottle.com")
(sha/where-am-i 34.164879 -118.405243 "Valley Village, CA")
(sha/bootstrap-straight)
(sha/setup-ace-window)
(sha/setup-company-mode)
(sha/setup-projectile)

(provide 'init)
;;; End init.el
