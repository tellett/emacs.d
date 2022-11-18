;;; init-org-mode.el --- Initialization code for org-mode.
;;; Commentary:

;;; Requires:
(eval-when-compile (require 'org))

;;; Code:
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook 'turn-on-font-lock)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defvar org-todo-state-map)
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (define-key org-todo-state-map "p"
       #'(lambda nil (interactive) (org-todo "IN-PROGRESS")))
     (define-key org-todo-state-map "r"
       #'(lambda nil (interactive) (org-todo "IN-REVIEW")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "o"
       #'(lambda nil (interactive) (org-todo "OBSOLETE")))
     (define-key org-todo-state-map "b"
       #'(lambda nil (interactive) (org-todo "BLOCKED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "IN-REVIEW" "DONE")))

(defvar org-agenda-custom-commands)
(setq org-agenda-custom-commands
      (quote (("d" todo "DELEGATED" nil)
              ("c" todo "DONE|OBSOLETE" nil)
              ("b" todo "BLOCKED" nil)
              ("W" agenda "" ((org-agenda-ndays 21)))
              ("A" agenda ""
               ((org-agenda-skip-function
                 (lambda nil
                   (org-agenda-skip-entry-if (quote notregexp)
                                             "\\=.*\\[#A\\]")))
                (org-agenda-ndays 1)
                (org-agenda-overriding-header "Today's Priority #A tasks: ")))
              ("u" alltodo ""
               ((org-agenda-skip-function
                 (lambda nil
                   (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                             (quote regexp) "\n]+>")))
                (org-agenda-overriding-header "Unscheduled TODO entries:"))))))

(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory)
      org-deadline-warning-days 14
      org-default-notes-file "~/org/notes.org"
      org-fast-tag-selection-single-key (quote expert)
      org-reverse-note-order t)

(defvar org-agenda-ndays)
(setq org-agenda-ndays 7)

(defvar org-agenda-show-all-dates)
(setq org-agenda-show-all-dates t)

(defvar org-agenda-skip-deadline-if-done)
(setq org-agenda-skip-deadline-if-done t)

(defvar org-agenda-skip-scheduled-if-done)
(setq org-agenda-skip-scheduled-if-done t)

(defvar org-agenda-start-on-weekday)
(setq org-agenda-start-on-weekday nil)

(defvar org-completion-use-ido)
(setq org-completion-use-ido t)

(defvar org-capture-templates)
(setq org-capture-templates
      (quote (("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
               "* TODO %?\n  %u")
              ("n" "Note" entry (file+datetree "~/org/notes.org")
               "** %u %?"))))

(provide 'init-org-mode)
;;; init-org-mode ends here
