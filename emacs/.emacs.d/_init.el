(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (require 'el-get nil 'noerror)
  (package-refresh-contents)
  (package-install 'el-get)
  (require 'el-get))

(require 'iso-transl)

;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;(el-get 'sync)

(add-to-list 'custom-theme-load-path "build/emacs-color-theme-solarized")

(add-to-list 'load-path "~/build")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

(set-frame-font "mononoki-10" nil t)

(find-file "~/stuff.org")
;(find-file "/mnt/win_data_hdd/sync/diary.org")
;(find-file "/mnt/win_data_hdd/sync/stuff.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(enable-recursive-minibuffers t)
 '(frame-background-mode (quote light))
 '(inhibit-startup-screen t)
 '(org-catch-invisible-edits (quote error))
 '(package-selected-packages (quote (el-get)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key [(control meta down)] (lambda () (interactive) (window-resize (selected-window) +1)))
(global-set-key [(control meta right)] (lambda () (interactive) (window-resize (selected-window) +1 1)))
(global-set-key [(control meta up)] (lambda () (interactive) (window-resize (selected-window) -1)))
(global-set-key [(control meta left)] (lambda () (interactive) (window-resize (selected-window) -1 1)))

(load-theme 'solarized t)
