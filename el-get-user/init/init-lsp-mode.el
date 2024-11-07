;;; init-lsp-mode.el -- initialization code for lsp-mode
;;; Commentary:
;;; Requires:
;;; Code:

;; (setq lsp-completion-provider :none)

(add-hook 'lsp-before-initialize-hook 'pet-mode)

;;; init-lsp-mode.el ends here
