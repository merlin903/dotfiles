(defun sha/who-am-i (name email)
  "Sets the `user-full-name' variable to NAME.
Sets `user-mail-address' to EMAIL."
  (interactive)
  (setq user-full-name name
	user-mail-address email))
