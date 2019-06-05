(require 'auto-highlight-symbol)

(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (auto-highlight-symbol-mode t))))
