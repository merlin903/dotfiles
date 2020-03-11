(defun sha/where-am-i (lat long location-name)
  "Tell Emacs where you're located"
  (setq calendar-latitude lat
	calendar-longitude long
	calendar-location-name location-name))
