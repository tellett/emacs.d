;;; init-git-gutter.el --- initialization code for git-gutter.

;;; Commentary:
;;; Requires:
;;; Code:


(eval-when-compile (require 'git-gutter))

(add-hook 'prog-mode-hook (lambda ()
                            (setq git-gutter:update-interval 0.02)))

(provide 'init-git-gutter)

;;; init-git-gutter.el ends here
