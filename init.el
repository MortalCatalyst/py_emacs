;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    js2-mode
    emmet-mode
    smartrep
    leuven-theme
    go-mode
    org-journal
    magit
    markdown-mode
    pbcopy
    py-autopep8
    rainbow-delimiters
    web-mode
    helm-projectile
    flx-ido
    pomodoro
    projectile))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'leuven t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
;;(add-to-list 'org-modules 'org-timer)

;; GTD
;;---------------------------------------

(setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(require 'org-journal)
(setq org-log-done 'time)
;; (eval-after-load 'org-agenda
;;   '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))

(setq org-use-effective-time t)

(defun my/org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
(setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)

(add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
(add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
(add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
(add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
(add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
(add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
(add-to-list 'org-speed-commands-user '("P" call-interactively 'org2blog/wp-post-subtree))
(add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
(add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
;; (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)
(setq org-agenda-files 
      (list "~/Dropbox/MyOrg/mygtd.org" "~/Dropbox/MyOrg/business.org" "~/Dropbox/MyOrg/personal.org"))
;;; TODO figure out the task timing/estimation

(defvar org-journal-dir "/home/sayth/Dropbox/MyOrg/"  
  "Path to OrgMode journal file.")  
(defvar org-journal-date-format "%Y-%m-%d"  
  "Date format string for journal headings.")  
  
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; C-c C-x C-i clock-in
;; C-c C-x C-o clock-out
;; C-c C-x C-e modify-effort-estimate
;; C-c C-x C-j clock-goto
;; C-c C-x C-t clock-report

(setq org-todo-keywords
 '((sequence
    "TODO(t)"  ; next action
    "TOBLOG(b)"  ; next action
    "STARTED(s)"
    "WAITING(w@/!)"
    "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@drawing" . ?d)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("fuzzy" . ?0)
                      ("highenergy" . ?1)))

(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

;; (use-package org
;;  :load-path "~/elisp/org-mode/lisp"
;;  :init
;;  (progn
;;   (setq org-clock-idle-time nil)
;;   (setq org-log-done 'time)
;;   (setq org-clock-continuously nil)
;;   (setq org-clock-persist t)
;;   (setq org-clock-in-switch-to-state "STARTED")
;;   (setq org-clock-in-resume nil)
;;   (setq org-show-notification-handler 'message)
;;   (setq org-clock-report-include-clocking-task t))
;;  :config
;;  (org-clock-persistence-insinuate))

;; (add-hook 'org-clock-in-prepare-hook
;;           'my/org-mode-ask-effort)

(defun my/org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

;;; helm-mini
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(require 'smartrep)

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )
;;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-hook 'python-mode-hook #'electric-spacing-mode)

(show-paren-mode 1)
(setq show-paren-style 'mixed) ; highlight brackets if visible, else entire expression




(require 'pomodoro) 
    (pomodoro-add-to-mode-line)

;;switch panes
;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)
;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(elpy-use-ipython)
(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'elpy-mode-hook
    (lambda ()
;;    (local-unset-key (kbd "C-c C-c"))
    (define-key elpy-mode-map (kbd "<f5>") 'elpy-shell-send-region-or-buffer)))
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; enable rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook `python-mode-hook `rainbow-delimiters-mode)

;; use flycheck not flymake
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; enable newline-and-indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; _javascript --- javascript configuration

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-highlight-level 3)
(setq-default js2-basic-offset 4)

;; _markdown -- markdown configuration

;; use gfm (github flavored markdown) supplied by markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json?\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-12-30"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (setq ξp1 (point-min) ξp2 (point-max))
      (if (use-region-p)
          (setq ξp1 (region-beginning) ξp2 (region-end))
        (setq ξp1 (line-beginning-position) ξp2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (forward-line 1)
          (end-of-line)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save ξp1 ξp2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))))

(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste

;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (electric-spacing org-gcal web-mode rainbow-delimiters py-autopep8 pbcopy markdown-mode magit go-mode js2-mode elpy ein material-theme better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
