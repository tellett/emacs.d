;;; init-exec-path-from-shell --- initialization code for exec-path-from-shell.
;;; Commentary:
;;; Code:

(eval-when-compile (require 'exec-path-from-shell))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

(dolist (var '("CLOUDSDK_HOME" "LSP_USE_PLISTS" "PYENV_ROOT"))
  (add-to-list 'exec-path-from-shell-variables var))

(when (or (memq window-system '(mac ns x)) (daemonp))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)
;;; init-exec-path-from-shell.el ends here
