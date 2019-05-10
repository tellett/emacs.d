(add-hook 'web-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends)
                                '(company-web-html))))
