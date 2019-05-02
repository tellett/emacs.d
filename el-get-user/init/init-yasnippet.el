(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions
      '(yas-popup-isearch-prompt
        yas-ido-prompt
        yas-no-prompt))

(setq yas-snippet-dirs '("~/src/opensource/personal/yasnippet-snippets"))

;; (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

;; this solves the "term-send-raw: Wrong type argument: characterp, tab" issue
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

(yas-global-mode 1)
