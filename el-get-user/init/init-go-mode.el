;;; init-go-mode.el --- initialization code for python-mode
;;; Commentary:
;;; Require:


;;; Code:


;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  ;; Set up before-save hooks to format buffer and add/delete imports.
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook
 'go-mode-hook
 (lambda ()

   ;; Company mode
   (set (make-local-variable 'company-backends) '(go-company))
   (setq company-idle-delay 0
         company-minimum-prefix-length 1)

   (setq gofmt-command "goimports"
         tab-width 2
         indent-tabs-mode 1
         fill-column 100)

   (add-to-list 'lsp-enabled-clients 'gopls)
   (lsp-deferred)
   ))

(add-hook 'go-mode-hook #'yas-minor-mode)


(provide 'init-go-mode)
;;; init-go-mode.el ends here
