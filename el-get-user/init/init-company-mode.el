;;; Require

(require 'company)
;; (require 'company-posframe)
(require 'company-yasnippet)
(require 'company-dabbrev)
(require 'company-files)
;; (require 'desktop)

;;; Code:

;; Config for company mode.
;; (global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
;; (setq company-show-numbers nil)

;; Customize company backends.
(push 'company-files company-backends)

;; Let desktop.el not record the company-posframe-mode
;; (company-posframe-mode 1)
;; (push '(company-posframe-mode . nil)
;;      desktop-minor-mode-table)

;; Add `company-elisp' backend for elisp.
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (require 'company-elisp)
             (push 'company-elisp company-backends)))

;; Key settings.
(define-key company-mode-map (kbd "TAB") nil)

(mapc #'(lambda (x) (define-key company-active-map (kbd x) nil))
      '("M-1" "M-2" "M-3"
        "M-4" "M-5" "M-6"
        "M-7" "M-8" "M-9"
        "M-0" "C-m"))

(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "M-h") 'company-complete-selection)
(define-key company-active-map (kbd "M-H") 'company-complete-common)
(define-key company-active-map (kbd "M-w") 'company-show-location)
(define-key company-active-map (kbd "M-s") 'company-search-candidates)
(define-key company-active-map (kbd "M-S") 'company-filter-candidates)
(define-key company-active-map (kbd "M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p") 'company-select-previous)

;; Add yasnippet support for all company backends.
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(add-hook 'after-init-hook 'global-company-mode)
