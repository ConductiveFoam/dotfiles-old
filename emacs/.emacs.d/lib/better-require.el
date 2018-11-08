;;; better-require.el ---
;;
;; Filename: better-require.el
;; Description: Improved require
;; Author: Jared D
;; Created: Mon Nov  5 11:39:28 2018 (+0100)
;; Last-Updated: Mon Nov  5 11:39:28 2018 (+0100)
;;           By: Sam B
;; URL: https://curiousprogrammer.wordpress.com/2010/08/09/avoiding-the-tyranny-of-other-peoples-decisions/
;; Keywords: require
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Improved require functionality
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(provide 'better-require)

(defun require* (feature &optional force)
  (when (or force (not (featurep feature)))
    (setq feature (symbol-name feature))
    (let ((path load-path)
	  (found-filename nil)
	  head el-attribs elc-attribs)
      (while (and (not found-filename) path)
	(setq head (pop path))
	(let ((el-filename (format "%s/%s.el" head feature))
	      (elc-filename (format "%s/%s.elc" head feature)))
	  ;; if .el and .elc both exist, pick the newest
	  ;; otherwise pick the one that exists if any
	  (cond ((and (file-exists-p el-filename)
		      (file-exists-p elc-filename))
		 (if (file-newer-than-file-p el-filename elc-filename)
		     (setq found-filename el-filename)
		   (setq found-filename elc-filename)))
		((file-exists-p el-filename)
		 (setq found-filename el-filename))
		((file-exists-p elc-filename)
		 (setq found-filename elc-filename)))
	  ;; load file if found
	  (when found-filename
	    (message (format "Found: [%s]" found-filename))
	    (let ((load-suffixes ()))
	      (load found-filename)))))
      (unless found-filename (error "Unable to find %s" feature)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; better-require.el ends here
