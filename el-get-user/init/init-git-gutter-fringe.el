;;; init-git-gutter-fringe.el --- initialization code for git-gutter-fringe.

;;; Commentary:
;;; Requires:
;;; Code:

(require 'git-gutter-fringe)
(global-git-gutter-mode t)

(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

(provide 'init-git-gutter-fringe)

;;; init-git-gutter-fringe.el ends here
