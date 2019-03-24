(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:foreground "black")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))

;;; Packages & local imports
(add-to-list 'load-path "~/.emacs.d/lib")
(require 'better-require)
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-selected-packages '(use-package smart-mode-line cyphejor smartparens company haskell-mode flycheck-rust racer cargo company-jedi))
(package-initialize)
(require 'use-package)
(require 'company) ;; Necessary since company-mode-map is referenced later

;;; Environment
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(setq select-enable-clipboard t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(menu-bar-mode -1)

;;; Auto-header
(require 'header2)

;;; Sensible defaults
(require 'sensible-defaults)
(sensible-defaults/increase-gc-threshold)
(sensible-defaults/delete-trailing-whitespace)
(sensible-defaults/treat-camelcase-as-separate-words)
(sensible-defaults/single-space-after-periods)
(sensible-defaults/ensure-that-files-end-with-newline)
(sensible-defaults/quiet-startup)
(sensible-defaults/make-dired-file-sizes-human-readable)
(sensible-defaults/shorten-yes-or-no)
(sensible-defaults/always-highlight-code)
(sensible-defaults/bind-commenting-and-uncommenting)

;;; Personal info
(setq user-full-name "Sam B"
      user-mail-address "nhalbleiter@gmail.com")

;; Mode Line
(require 'modeline-posn)
(column-number-mode)
(setq modelinepos-column-limit 90)
(use-package smart-mode-line
  :ensure t
  :custom
  (sml/replacer-regexp-list
   '(("^~/org" ":Org:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/[Gg]it/" ":Git:")
     ("^~/[Gg]it[Hh]ub/" ":Git:")
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
     ("^~/dotfiles/" ":DOT:")
     ("^:DOT:\\([^/]+\\)/" ":DOT:\\1:")
     ("^:DOT:emacs:\\.emacs\\.d/" ":DOT:ED:")))
  (sml/theme 'light)
  :config
  (sml/setup))
(use-package cyphejor
  :ensure t
  :custom
  (cyphejor-rules
   '(:upcase
     ("emacs" "e")
     ("lisp" "λ")
     ("haskell" "λ=")
     ("rust" "rs")
     ("cargo" "")
     ("company" "")
     ("racer" "")
     ("python" "Py")
     ("org" "ORG")
     ("interaction" "i-" :prefix)
     ("interactive" "i-" :prefix)
     ("mode" "")))
  :config
  (cyphejor-mode))

;;; Smartparens
;; Auxiliary
(defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))
(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

;; Config
(use-package smartparens-config
  :ensure smartparens
  :hook ((prog-mode org-mode) . smartparens-mode))
(show-smartparens-global-mode t) ;; Would not work in :config

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)
 ("C-M-n" . sp-down-sexp)
 ("C-M-p" . sp-up-sexp)
 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)
 ("M-p" . sp-previous-sexp)
 ("M-n" . sp-next-sexp)
 ("C-c M-m f" . sp-forward-symbol)
 ("C-c M-m b" . sp-backward-symbol)
 ("C-c M-m k" . sp-forward-barf-sexp)
 ("C-c M-m l" . sp-forward-slurp-sexp)
 ("C-c M-m j" . sp-backward-barf-sexp)
 ("C-c M-m h" . sp-backward-slurp-sexp)
 ("C-c M-m t" . sp-transpose-sexp)
 ("C-c M-m u" . sp-unwrap-sexp)
 ("C-c M-m z" . sp-unwrap-backward-sexp)
 ("C-M-d" . sp-kill-hybrid-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-M-h" . sp-backward-kill-sexp)

 ("C-c M-w (" . wrap-with-parens)
 ("C-c M-w [" . wrap-with-brackets)
 ("C-c M-w {" . wrap-with-braces)
 ("C-c M-w '" . wrap-with-single-quotes)
 ("C-c M-w \"" . wrap-with-double-quotes)
 ;;("C-c M-w _" . wrap-with-underscores)
 ("C-c M-w `" . wrap-with-back-quotes)

 ("C-c C-p s" . sp-prefix-symbol-object)
 ("C-c C-p t" . sp-prefix-tag-object)
 ("C-c C-p p" . sp-prefix-pair-object))

;;; Movement helpers
(defun move-to-window-line-top ()
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-center ()
  (interactive)
  (move-to-window-line nil))

(defun move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -1))

;;; Global keybinds
(bind-keys
 :map global-map
 ;; Window resizing
 ("C-c C-w k" . (lambda () (interactive) (window-resize (selected-window) -1)))
 ("C-c C-w j" . (lambda () (interactive) (window-resize (selected-window) +1)))
 ("C-c C-w h" . (lambda () (interactive) (window-resize (selected-window) +1 1)))
 ("C-c C-w l" . (lambda () (interactive) (window-resize (selected-window) -1 1)))

 ;; Navigation
 ("C-c M-m <" . move-to-window-line-top)
 ("C-c M-m |" . move-to-window-line-center)
 ("C-c M-m >" . move-to-window-line-bottom)

 ;; Miscellaneous
 ("C-c l" . org-store-link))

;;; Prog mode
(add-hook 'prog-mode-hook 'company-mode)

(bind-keys
 :map prog-mode-map
 ("C-c ;" . comment-or-uncomment-region))
(bind-keys
 :map company-mode-map
 ("TAB" . indent-for-tab-command)
 ("M-TAB" . company-manual-begin))

;;; Timestamp insertion
(defun format-date (format)
  (let ((system-time-locale "en_US.UTF-8"))
    (insert (format-time-string format))))

(defun insert-date ()
  (interactive)
  (format-date "%A, %B %d %Y"))
(defun insert-date-and-time ()
  (interactive)
  (format-date "%Y-%m-%d %H:%M:%S"))

(bind-keys
 :map global-map
 ("C-c d" . insert-date)
 ("C-c C-d" . insert-date-and-time))

;;; Additional file extensions for modes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;;; Org mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-hide-leading-stars t)

;;; Haskell
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 (append '((company-capf company-dabbrev-code))
			 company-backends))))

;;; Rust
(use-package rust-mode
  :ensure t
  :init
  (progn
    (add-hook 'rust-mode-hook #'cargo-minor-mode)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (setq company-tooltip-align-annotations t)))
(bind-keys
 :map rust-mode-map
 ("C-c TAB" . rust-format-buffer))

;;; Open a few often used files
(find-file "~/stuff.org")
(find-file "~/dotfiles/xmonad/.xmonad/xmonad.hs")
(find-file "~/dotfiles/emacs/.emacs.d/init.el")
(find-file "~/dotfiles/README.org")
