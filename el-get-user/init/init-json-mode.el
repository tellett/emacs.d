(autoload 'json-mode "json-mode" "Major mode for editing JSON files" t)
(add-to-list 'auto-mode-alist '("\\.avro\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
