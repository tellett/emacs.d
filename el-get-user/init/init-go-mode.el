(dolist (hook '(go-mode-hook))
  (add-hook hook '(lambda () (auto-fill-mode nil))))
