(autoload 'docker-compose-mode "docker-compose-mode" "Major mode for editing docker-compose files." t)
(add-to-list 'auto-mode-alist '("docker-compose\\.yml\\'" . docker-compose-mode))
(add-to-list 'auto-mode-alist '("docker-compose\\.yaml\\'" . docker-compose-mode))
