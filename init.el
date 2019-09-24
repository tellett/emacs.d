;; (package-initialize)

(require 'cl-lib)

(add-to-list 'exec-path "/usr/local/bin")

;; these are shoved at the top to speed boot.
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(when (string-equal window-system "x")
  (setq redisplay-dont-pause t)
  ;; (fringe-mode '(0 . 8))
  ;; (scroll-bar-mode 0)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-8"))

  ;; set chrome as the default browser
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "google-chrome")

  ;; let's map a few of the Evoluent's buttons.
  (global-set-key [mouse-8] '(lambda ()
                               (interactive)
                               (next-buffer)))
  (global-set-key [mouse-10] '(lambda ()
                                (interactive)
                                (previous-buffer))))

(blink-cursor-mode t)
(column-number-mode t)
(electric-pair-mode t)
(global-auto-revert-mode)
(global-hl-line-mode t)
(line-number-mode t)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; use the echo area for tooltips
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; disable the "... of ..." to save space on the mode line
(size-indication-mode -1)

;; highlight region and comment
(global-set-key (kbd "\C-c\C-c") 'comment-or-uncomment-region)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; (defun split-window-prefer-horizonally (window)
;;   "If there's only one window (excluding any possibly active
;;          minibuffer), then split the current window horizontally."
;;   (if (and (one-window-p t)
;;            (not (active-minibuffer-window)))
;;       (let ((split-height-threshold nil))
;;         (split-window-sensibly window))
;;     (split-window-sensibly window)))
;; (setq split-window-preferred-function 'split-window-prefer-horizonally)


(setq exec-path (append '("/usr/local/go/bin" "/home/tellett/bin")
                        exec-path))

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; --------------------------------------------------------------------------
;; ansi-term

;; kill buffers that are finished
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; force bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; use utf8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)


;; --------------------------------------------------------------------------
;; autosaves

;; Auto-save more often
(setq auto-save-interval 150)
(setq auto-save-timeout 20)

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir (concat user-emacs-directory
                             (convert-standard-filename "autosaves/")))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Numbered backups
(setq version-control t
      kept-old-versions 3
      kept-new-versions 10
      trim-versions-without-asking t
      delete-old-versions t)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat user-emacs-directory
                           (convert-standard-filename "backups/")))
(setq backup-directory-alist (list (cons "." backup-dir)))


;; --------------------------------------------------------------------------
;; auto-fill-mode

(setq-default fill-column 78)

;; Auto-fill-mode the the automatic wrapping of lines and insertion of
;; newlines when the cursor goes over the column limit.

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda ()
                   (auto-fill-mode 1))))


;; --------------------------------------------------------------------------
;; compilation-mode

(setq-default compilation-scroll-output 'first-error)

(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; --------------------------------------------------------------------------
;; clean the mode line

(defvar mode-line-cleaner-alist
  `((abbrev-mode . "")
    (auto-complete-mode . " α")
    (auto-fill-function . " F")
    (eldoc-mode . "")
    (yas-minor-mode . " υ")
    (yas-global-mode . " υ")
    (paredit-mode . " π")
    ;; Major modes
    (compilation-mode . "Comp")
    (emacs-lisp-mode . "EL")
    (hi-lock-mode . "")
    (lisp-interaction-mode . "λ")
    (nxhtml-mode . "nx")
    (python-mode . "Py")
    )
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))


;; --------------------------------------------------------------------------
;; desktop-save-mode

;;;; Very important code to save desktop state between Emacs sessions.
(require 'desktop)

(savehist-mode 1)

(defun save-emacs-state ()
  "Saves the desktop as of right now, so if it dies it'll come back in
  the right place."
  (interactive)
  (desktop-save desktop-dirname)
  (message "Saved desktop %s" desktop-dirname))

;; Save the desktop state after a bunch of idle time
(defvar save-emacs-timer
  (run-with-idle-timer 300 t #'save-emacs-state)
  "Timer that saves my desktop state every 5 minutes.")

;; Augment shell-mode to save state (history and current directory) on desktop
;; save
(defun my-shell-save-desktop-data (desktop-dirname)
  "Extra info for shell-mode buffers to be saved in the desktop file."
  (list default-directory comint-input-ring))

(defun my-shell-restore-desktop-buffer
  (desktop-buffer-file-name desktop-buffer-name desktop-buffer-misc)
  "Restore a shell buffer's state from the desktop file."
  (let ((dir (nth 0 desktop-buffer-misc))
        (ring (nth 1 desktop-buffer-misc)))
    (when desktop-buffer-name
      (set-buffer (get-buffer-create desktop-buffer-name))
      (when dir
        (setq default-directory dir))
      (shell desktop-buffer-name)
      (when ring
        (setq comint-input-ring ring))
      (current-buffer))))

(defun my-shell-setup-desktop ()
  "Sets up a shell buffer to have its state saved in the desktop file."
  (set (make-local-variable 'desktop-save-buffer) #'my-shell-save-desktop-data))

(add-to-list 'desktop-buffer-mode-handlers
             '(shell-mode . my-shell-restore-desktop-buffer))
(add-hook 'shell-mode-hook #'my-shell-setup-desktop)

(setq desktop-dirname (expand-file-name "~/"))
(setq desktop-path (list desktop-dirname))

(when (desktop-owner)
  (let* ((attrs (process-attributes (desktop-owner)))
         (usercons (and attrs (assq 'user attrs)))
         (user (and usercons (cdr usercons))))
    (if (equal user (user-login-name))
        ;; Best guess is that the old emacs is still there
        (setq desktop-load-locked-desktop (if (daemonp) nil 'ask))
      ;; It's dead, Jim.
      (delete-file (desktop-full-lock-name)))))

;; bugfix in desktop.el
(defadvice desktop-create-buffer (around dont-bury-dead-buffers act)
  (cl-flet ((real-bury-buffer () nil))
    (fset 'real-bury-buffer (symbol-function #'bury-buffer))
    (cl-flet ((bury-buffer (buf) (when (buffer-live-p buf)
                                (real-bury-buffer buf))))
      ad-do-it)))

;; don't ever prompt while restoring desktop
(defadvice desktop-read (around no-prompting act)
  (cl-flet ((y-or-n-p (prompt) nil)
         (read-char-choice
          (prompt chars suppress-quit)
          (if (memq ?n chars)
              ?n
            (error "Don't know how to fake read-char-choice"))))
    ad-do-it))

(desktop-save-mode 1)


;; --------------------------------------------------------------------------
;; electric pairs

;; TODO: change this to walk over a closing brace...
(defun electric-pair ()
  "Insert character pair without surrounding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda ()
                   (electric-pair-mode t)
                   (define-key prog-mode-map "\"" 'electric-pair)
                   (define-key prog-mode-map "\'" 'electric-pair)
                   (define-key prog-mode-map "(" 'electric-pair)
                   (define-key prog-mode-map "[" 'electric-pair)
                   (define-key prog-mode-map "{" 'electric-pair))))


;; --------------------------------------------------------------------------
;; flyspell-mode

(setq ispell-personal-dictionary "~/.ispell-personal-dict")

;; Use aspell if installed
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list"))

;; enable for the following modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;; disable for the following modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; --------------------------------------------------------------------------
; html-mode

(add-hook 'html-mode-hook (lambda () (auto-fill-mode -1) (zencoding-mode 1)))

;; --------------------------------------------------------------------------
; ido-mode

(ido-mode t)
(ido-everywhere t)

(setq ido-case-fold  t                 ; be case-insensitive
      ido-confirm-unique-completion t  ; wait for RET, period.
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

      ido-enable-flex-matching t          ; be smart
      ido-enable-last-directory-history t ; remember last used dirs
      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido"
                           "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*"
                           "^\*")
      ido-ignore-extensions t          ; ignore stuff like .o files, etc
      ido-max-work-directory-list 30   ; should be enough
      ido-max-work-file-list      50   ; remember many
      ido-use-filename-at-point nil    ; don't use filename at point (annoying)
      ido-use-url-at-point nil         ; don't use url at point (annoying)
      ido-max-prospects 8              ; don't spam my minibuffer
      ;; ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src"))
      )

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)


;; --------------------------------------------------------------------------
;; linum-mode

(add-hook 'linum-before-numbering-hook
          (lambda ()
            (let ((w (length (number-to-string
                              (count-lines (point-min) (point-max))))))
              (setq linum-format
                    `(lambda (line)
                       (propertize (concat
                                    (truncate-string-to-width
                                     "" (- ,w (length (number-to-string line)))
                                     nil ?\x2007)
                                    (number-to-string line)
                                    "\x2007")
                                   'face 'linum))))))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda ()
                   (linum-mode 1))))


;; --------------------------------------------------------------------------
;; python-mode

;;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-m" 'newline-and-indent)
            (define-key python-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)))


;; --------------------------------------------------------------------------
;; text-mode

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; --------------------------------------------------------------------------
;; sh-mode

(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)
                          (setq indent-tabs-mode nil)))


;; --------------------------------------------------------------------------
;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; --------------------------------------------------------------------------
;; whitespace-mode

(setq
 ;; make whitespace-mode use “¶” for newline and “▷” for tab.
 ;; together with the rest of its defaults
 ;; source: http://xahlee.org/emacs/whitespace-mode.html
 whitespace-display-mappings '((space-mark 32 [183] [46]) ; normal space, ·
                               (space-mark 160 [164] [95])
                               (space-mark 2208 [2212] [95])
                               (space-mark 2336 [2340] [95])
                               (space-mark 3616 [3620] [95])
                               (space-mark 3872 [3876] [95])
                               (newline-mark 10 [182 10]) ; newlne, ¶
                               (tab-mark 9 [9655 9] [92 9])) ; tab, ▷

 ;; make whitespace-mode use just basic coloring
 whitespace-style '(lines
                    lines-tail
                    newline
                    newline-mark
                    spaces
                    space-mark
                    tabs
                    tab-mark
                    trailing))


;; --------------------------------------------------------------------------
;; winner-mode

(require 'winner)
(winner-mode 1)


;; --------------------------------------------------------------------------
;; el-get

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(setq el-get-git-install-url "https://github.com/tellett/el-get.git"
      el-get-user-directory (concat user-emacs-directory "el-get-user")
      el-get-user-package-directory (concat (file-name-as-directory
                                             el-get-user-directory) "init")
      the-el-get-packages '(all-the-icons
                            auto-highlight-symbol
                            bazel-mode
                            company-mode
                            company-quickhelp
                            company-web
                            deft
                            docker-compose-mode
                            docker-tramp
                            dockerfile-mode
                            exec-path-from-shell
                            flycheck
                            go-company
                            go-dlv
                            go-eldoc
                            go-mode
                            go-projectile
                            go-rename
                            ido-completing-read-plus
                            json-mode
                            json-reformat
                            magit
                            markdown-mode
                            neotree
                            org-mode
                            popup
                            pos-tip
                            projectile
                            protobuf-mode
                            puppet-mode
                            rainbow-delimiters
                            rainbow-mode
                            scala-mode
                            smex
;;                            smooth-scrolling
                            spaceline
;;                            spaceline-all-the-icons
                            sr-speedbar
                            tomorrow-theme
                            web-completion-data
                            web-mode
                            yaml-mode
                            yasnippet
                            yasnippet-snippets
                            zencoding-mode))

(eval-after-load "el-get/el-get.el"
  (lambda()
    '((add-to-list 'el-get-recipe-path
                   (concat el-get-user-directory "recipes")))))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/tellett/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(mapcar (lambda(x) (add-to-list 'package-archives x))
        '(
;;	  ("marmalade" . "https://marmalade-repo.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("tromey" . "http://tromey.com/elpa/")))

(el-get 'sync '(el-get))
(el-get 'sync the-el-get-packages)

;; --------------------------------------------------------------------------
;; compile and load up the local stuff

(setq custom-file (concat user-emacs-directory "init-local.el"))
(if (and (file-exists-p custom-file) (file-readable-p custom-file))
    (byte-compile-file custom-file t))
(put 'upcase-region 'disabled nil)
