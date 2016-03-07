
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
(setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
(require 'org-journal)
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

;; GTD
;;---------------------------------------
(defun gtd ()
   (interactive)
   (find-file "/home/sayth/Dropbox/MyOrg/mygtd.org")
   ) ;;; locate my gtd file quickly
;;; C-c a h (to view outline of day)
;; ("H" "Office and Home Lists"
;;      ((agenda)
;;           (tags-todo "OFFICE")
;;           (tags-todo "HOME")
;;           (tags-todo "COMPUTER")
;;           (tags-todo "STUDY")
;;           (tags-todo "READING")))

;; ("D" "Daily Action List"
;;       (
;;            (agenda "" ((org-agenda-ndays 1)
;;                        (org-agenda-sorting-strategy
;;                         (quote ((agenda time-up priority-down tag-up) )))
;;                        (org-deadline-warning-days 0)
;;                        ))))

;;; TODO figure out the task timing/estimation

(defvar org-journal-dir "/home/sayth/Dropbox/MyOrg/"  
  "Path to OrgMode journal file.")  
(defvar org-journal-date-format "%Y-%m-%d"  
  "Date format string for journal headings.")  
  

;; C-c C-x C-i clock-in
;; C-c C-x C-o clock-out
;; C-c C-x C-e modify-effort-estimate
;; C-c C-x C-j clock-goto
;; C-c C-x C-t clock-report

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
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
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
