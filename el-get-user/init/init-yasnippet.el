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

(setq yas-snippet-dirs '("~/src/personal/yasnippet-snippets"))

;; (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

;; this solves the "term-send-raw: Wrong type argument: characterp, tab" issue
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

(require 'yasnippet)

(yas-global-mode 1)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))


(global-set-key [tab] 'tab-indent-or-complete)
