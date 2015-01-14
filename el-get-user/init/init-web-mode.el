;; docs: http://web-mode.org
;; code: https://github.com/fxbois/web-mode

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2
        web-mode-auto-close-style 2
        web-mode-enable-auto-quoting t
        web-mode-enable-auto-pairing t

        ;; <style> stuff
        web-mode-css-indent-offset 2
        web-mode-style-padding 2

        ;; <script> stuff
        web-mode-code-indent-offset 2
        web-mode-script-padding 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
