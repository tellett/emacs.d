;;; init-python-mode.el --- initialization code for python-mode
;;; Commentary:
;;; Require:

(eval-when-compile
  (cond ((locate-library "pet")
         (require 'pet))))

(eval-when-compile
  (cond ((locate-library "dap-mode")
         (require 'dap-mode))))

(eval-when-compile
  (cond ((locate-library "lsp-mode")
         (require 'lsp-mode))))

;; (eval-when-compile
;;   (cond ((locate-library "python-black")
;;          (require 'python-black))))

;; (eval-when-compile
;;   (cond ((locate-library "python-isort")
;;          (require 'python-isort))))

(eval-when-compile
  (cond ((locate-library "python-pytest")
         (require 'python-pytest))))

(eval-when-compile
  (cond ((locate-library "lsp-pylsp")
         (require 'lsp-pylsp))))

;;; Code:

(defface font-lock-operator-face
  '((((class color) (min-colors 89)) :inherit 'font-lock-preprocessor-face))
  "Face for highlighting python operators"
  :group 'python-mode)

; (add-hook 'python-mode-hook 'pet-flycheck-setup)

(add-hook
 'python-mode-hook
 (lambda ()
   (setq fill-column 120)

   (define-key python-mode-map "\C-m" 'newline-and-indent)
   (define-key python-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)

   (cond ((locate-library "pet")
          (require 'pet)
          (pet-mode 1)

          (setq-local python-shell-interpreter (pet-executable-find "python")
                      python-shell-virtualenv-root (pet-virtualenv-root))

          (pet-flycheck-setup)))

   (flycheck-mode 1)

   (cond ((locate-library "dap-mode")
          (defvar dap-python-executable)
          (defvar python-shell-interpreter)
          (setq-local dap-python-executable python-shell-interpreter)))

   (cond ((locate-library "python-pytest")
          (setq-local python-pytest-executable (pet-executable-find "pytest"))))

   (cond ((locate-library "lsp-mode")
          (require 'lsp-mode)
          (defvar lsp-enabled-clients)
          (defvar lsp-disabled-clients)
          (defvar lsp-auto-configure)

          (setq lsp-auto-configure 1)

          (cond ((locate-library "lsp-pylsp")
                 (require 'lsp-pylsp)

                 (defvar lsp-pylsp-plugins-ruff-executable)
                 (setq-local lsp-pylsp-plugins-ruff-executable (pet-executable-find "ruff"))

                 (add-to-list 'lsp-enabled-clients 'pylsp)

                 (defvar lsp-pylsp-plugins-autopep8-enabled)
                 (defvar lsp-pylsp-plugins-black-enabled)
                 (defvar lsp-pylsp-plugins-flake8-enabled)
                 (defvar lsp-pylsp-plugins-isort-enabled)
                 (defvar lsp-pylsp-plugins-mccabe-enabled)
                 (defvar lsp-pylsp-plugins-mypy-enabled)
                 (defvar lsp-pylsp-plugins-mypy-live-mode)
                 (defvar lsp-pylsp-plugins-pycodestyle-enabled)
                 (defvar lsp-pylsp-plugins-pyflakes-enabled)
                 (defvar lsp-pylsp-plugins-pylint-enabled)

                 (setq
                  lsp-pylsp-plugins-autopep8-enabled nil
                  lsp-pylsp-plugins-black-enabled nil
                  lsp-pylsp-plugins-flake8-enabled nil
                  lsp-pylsp-plugins-isort-enabled nil
                  lsp-pylsp-plugins-mccabe-enabled nil
                  lsp-pylsp-plugins-mypy-enabled t
                  lsp-pylsp-plugins-mypy-live-mode t
                  lsp-pylsp-plugins-pycodestyle-enabled nil
                  lsp-pylsp-plugins-pyflakes-enabled nil
                  lsp-pylsp-plugins-pylint-enabled nil
                  )))

          (cond ((locate-library "lsp-jedi")
                 (require 'lsp-jedi)
                 (add-to-list 'lsp-enabled-clients 'jedi)
                 (defvar lsp-jedi-executable-command)
                 (setq-local lsp-jedi-executable-command
                             (pet-executable-find "jedi-language-server"))))

          (lsp-deferred)))

   ;; (cond ((locate-library "python-black")
   ;;        (when-let
   ;;            ((black-executable (pet-executable-find "black")))
   ;;          (defvar python-black-command)
   ;;          (setq-local python-black-command black-executable)
   ;;          (python-black-on-save-mode 1))))

   ;; (cond ((locate-library "python-isort")
   ;;        (when-let
   ;;            ((isort-executable (pet-executable-find "isort")))
   ;;          (defvar python-isort-command)
   ;;          (setq-local python-isort-command isort-executable)
   ;;          (python-isort-on-save-mode 1))))

   (cond ((locate-library "python-pytest")
          (defvar python-pytest-executable)
          (setq-local python-pytest-executable (pet-executable-find "pytest"))))))

(add-hook 'python-base-mode-hook 'pet-mode -10)

(provide 'init-python-mode)
;;; init-python-mode.el ends here
