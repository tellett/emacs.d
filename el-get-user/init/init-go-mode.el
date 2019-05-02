(dolist (hook '(go-mode-hook))
  (add-hook 'hook '(lambda ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (auto-complete-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq
   gofmt-command "goimports"
   tab-width 2
   indent-tabs-mode 1
   fill-column 100)
  )))
