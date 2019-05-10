(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (auto-highlight-symbol-mode t))))

;; (setq ahs-idle-interval 0.5)
