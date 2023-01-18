;;; init-direnv.el -- initialization code for direnv.
;;; Commentary:
;;; Code:

(eval-when-compile (require 'direnv))
(eval-when-compile (require 'exec-path-from-shell))

(progn (exec-path-from-shell-initialize))
(direnv-mode 1)

(provide 'init-direnv)
;;; init-direnv.el ends here
