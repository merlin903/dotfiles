;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(if (file-exists-p "~/.emacs.d/per-system-settings.el")
    (load-file "~/.emacs.d/per-system-settings.el"))

(defvar sa/is-ish nil
  "Determine if current system is iSH shell on an iPhone.")
(defvar sa/is-darwin nil
  "Determine if current system is Darwin based.")

(require 'subr-x)
(setq sa/is-ish
      (> (string-match-p "iSH" (string-trim (shell-command-to-string "uname -a"))) 0))

(setq sa/is-darwin
      (> (string-match-p "Darwin" (string-trim (shell-command-to-string "uname -a"))) 0))

;; Keep transient cruft out of ~/.emacs.d
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Add advice to automatically refresh packages
(defvar sa/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")

(defun sa/package-refresh (&rest args)
  "Refresh package metadata, if needed.
Ignores `ARGS'."
  (unless (eq sa/packages-refreshed t)
    (progn
      (package-refresh-contents)
      (setq sa/packages-refreshed t))))

(advice-add 'package-install :before #'sa/package-refresh)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Ensure packages by default
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

;; Add my elisp path to load-path
(push "~/.emacs.d/elisp" load-path)

(defun platform-keyword-to-string (platform-keyword)
  "Helper function for changing OS platform keywords to system-type strings"
  (cond
   ((eq platform-keyword 'widnows) "windows-nt")
   ((eq platform-keyword 'cygwin) "cygwin")
   ((eq platform-keyword 'osx) "darwin")
   ((eq platform-keyword 'linux) "gnu/linux")))

(defmacro on-platform-do (&rest platform-expressions)
  "Runs an elisp expression only on a particular platform"
  `(cond
    ,@(mapcar
       (lambda (platform-expr)
	 (let ((keyword (nth 0 platform-expr))
	       (expr (nth 1 platform-expr)))
	   `(,(if (listp keyword)
		  `(or
		    ,@(mapcar
		       (lambda (kw) `(string-equal system-type ,(platform-keyword-to-string kw)))
		       keyword))
		`(string-equal system-type ,(platform-keyword-to-string keyword)))
	     ,expr)))
       platform-expressions)))

(server-start)
