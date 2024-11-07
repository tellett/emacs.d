;;; init-pip-requirements.el -- initialization code for pip-requirements.
;;; Commentary:
;;; Code:


(eval-when-compile (require 'pip-requirements))

(add-to-list 'auto-mode-alist
             `(,(rx "requirements" (zero-or-more anything) ".in" string-end) . pip-requirements-mode))


(add-hook 'pip-requirements-mode-hook
          (lambda ()
            (setq pip-requirements-index-url
                  "https://us-python.pkg.dev/jarvis-ml-dev/python-ro/simple/")))

(provide 'init-pip-requirements)
;;; init-pip-requirements.el ends here
