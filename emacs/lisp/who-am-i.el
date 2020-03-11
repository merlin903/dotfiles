(defun sha/who-am-i (name email)
  "Sets the `user-full-name' variable to NAME and
sets `user-mail-address' to EMAIL."
  (setq user-full-name name
	user-mail-address email))
