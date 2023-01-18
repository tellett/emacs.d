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

(eval-when-compile
  (cond ((locate-library "python-black")
         (require 'python-black))))

(eval-when-compile
  (cond ((locate-library "python-isort")
         (require 'python-isort))))

(eval-when-compile
  (cond ((locate-library "python-pytest")
         (require 'python-pytest))))

(eval-when-compile (cond ((locate-library "lsp-pyright")
                          (require 'lsp-pyright))))

(eval-when-compile (cond ((locate-library "lsp-pyright")
                          (require 'lsp-pyright))))

;;; Code:

(defface font-lock-operator-face
  '((((class color) (min-colors 89)) :inherit 'font-lock-preprocessor-face))
  "Face for highlighting python operators"
  :group 'python-mode)

(add-hook
 'python-mode-hook
 (lambda ()
   (setq fill-column 88)

   (define-key python-mode-map "\C-m" 'newline-and-indent)
   (define-key python-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)

   (pet-mode)

   (setq-local python-shell-interpreter (pet-executable-find "python")
               python-shell-virtualenv-root (pet-virtualenv-root))

   (defvar flycheck-flake8-maximum-line-length)
   (setq flycheck-flake8-maximum-line-length 88)

   (pet-flycheck-setup)
   (flycheck-mode 1)

   (python-black-on-save-mode 1)
   (python-isort-on-save-mode 1)

   (cond ((locate-library "dap-mode")
          (defvar dap-python-executable)
          (defvar python-shell-interpreter)
          (setq-local dap-python-executable python-shell-interpreter)))

   (cond ((locate-library "lsp-mode")
          (defvar lsp-disabled-clients)
          (defvar lsp-enabled-clients)

          (cond ((locate-library "lsp-jedi")
                 (require 'lsp-jedi)
                 (add-to-list 'lsp-enabled-clients 'jedi)
                 (add-to-list 'lsp-disabled-clients 'pyls)
                 (add-to-list 'lsp-disabled-clients 'pylsp)
                 (defvar lsp-jedi-executable-command)
                 (setq-local lsp-jedi-executable-command
                             (pet-executable-find "jedi-language-server"))))

          (cond ((locate-library "lsp-pyright")
                 (require 'lsp-pyright)
                 (defvar lsp-pyright-python-executable-cmd)
                 (defvar lsp-pyright-venv-path)
                 (defvar python-shell-interpreter)
                 (defvar python-shell-virtualenv-root)
                 (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                             lsp-pyright-venv-path python-shell-virtualenv-root)))

          (lsp-deferred)))

   (cond ((locate-library "python-black")
          (when-let
              ((black-executable (pet-executable-find "black")))
            (defvar python-black-command)
            (setq-local python-black-command black-executable)
            (python-black-on-save-mode 1))))

   (cond ((locate-library "python-isort")
          (when-let
              ((isort-executable (pet-executable-find "isort")))
            (defvar python-isort-command)
            (setq-local python-isort-command isort-executable)
            (python-isort-on-save-mode 1))))

   (cond ((locate-library "python-pytest")
          (defvar python-pytest-executable)
          (setq-local python-pytest-executable (pet-executable-find "pytest"))))))

(provide 'init-python-mode)
;;; init-python-mode.el ends here
