(autoload 'terraform-mode "terraform-mode" "Major mode for editing Terraform files" t)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
