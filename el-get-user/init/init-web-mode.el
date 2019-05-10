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
        web-mode-script-padding 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
