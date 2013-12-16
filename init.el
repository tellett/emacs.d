;; these are shoved at the top to speed boot.
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode t)
(column-number-mode t)
(electric-pair-mode t)
(global-linum-mode t) ;; TODO add an extra space in the terminal
(hl-line-mode t)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; highlight region and comment
(global-set-key (kbd "\C-c\C-c") 'comment-or-uncomment-region) 

;; --------------------------------------------------------------------------
; ido-mode

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-max-prospects 10)

;; --------------------------------------------------------------------------
;; el-get

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(setq el-get-git-install-url "https://github.com/tellett/el-get.git"
      el-get-sources '((:name deft)
		       (:name magit)
		       (:name org) ;; TODO configure org-mode
		       (:name powerline)
		       (:name tomorrow-theme)
                       (:name yasnippet))
      el-get-user-directory (concat user-emacs-directory "el-get-user")
      el-get-user-package-directory (concat
				     (file-name-as-directory el-get-user-directory)
				     "init"))

(eval-after-load "el-get/el-get.el"
  '(add-to-list 'el-get-recipe-path
		(concat el-get-user-directory "recipes")))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/tellett/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync '(el-get))

(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

;; --------------------------------------------------------------------------
;; emacs customization area

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tomorrow-night)))
 '(custom-safe-themes (quote ("1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
