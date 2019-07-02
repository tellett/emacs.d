(setq godef-command "/home/tellett/go-code/bin/godef"
      godoc-command "go doc")

(dolist (hook '(go-mode-hook))
  (add-hook 'hook '(lambda ()
                     (local-set-key (kbd "M-.") 'godef-jump)
                     (add-hook 'before-save-hook 'gofmt-before-save)
                     (set (make-local-variable 'company-backends) '(go-company))
                     (setq gofmt-command "goimports"
                           tab-width 2
                           indent-tabs-mode 1
                           fill-column 100))))
