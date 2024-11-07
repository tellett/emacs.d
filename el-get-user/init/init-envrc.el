(require 'envrc)

(envrc-global-mode)

(add-hook 'change-major-mode-after-body-hook 'envrc-mode)
