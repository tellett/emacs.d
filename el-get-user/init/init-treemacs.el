;;; init-treemacs.el -- initialization code for treemacs.
;;; Commentary:
;;; Code:

(eval-when-compile (require 'treemacs))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1]
    #'treemacs-single-click-expand-action)
  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-git-commit-diff-mode t))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
