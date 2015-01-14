(dolist (hook '(go-mode-hook))
  (add-hook hook '(lambda ()
                    (setq-local fill-column 200))))
