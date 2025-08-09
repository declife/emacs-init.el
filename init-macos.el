;;; Start of init.el  -*- lexical-binding: t; -*-

;;Set up package management
(require 'package)

;;; code:

;;package
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;; Analyze Emacs startup performance
;; (use-package benchmark-init
;;   :ensure t
;;   :init (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/project/document/alpine.org"))
 '(package-selected-packages
   '(avy company flycheck helm magit mozc projectile vterm yasnippet
         zenburn-theme)))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; themes
(load-theme 'zenburn t)

;; magit mode
;(setq magit-ediff-dwim-show-on-hunks nil)
(setq ediff-split-window-function 'split-window-vertically)
(setq ediff-merge-split-window-function 'split-window-vertically)


;; General settings
(setq inhibit-startup-message t)       ;; Disable the startup message
(tool-bar-mode -1)                     ;; Disable the toolbar
(menu-bar-mode -1)                     ;; Disable the menu bar
(scroll-bar-mode -1)                   ;; Disable the scroll bar
(global-display-line-numbers-mode t)   ;; Show line numbers
(setq column-number-mode t)   ;; Show colume numbers
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Better buffer management

;; Set up a file backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      backup-by-copying t    ;; Avoid symlinks
      version-control t      ;; Use version numbers on backups
      delete-old-versions t  ;; Delete excess backups
      kept-new-versions 20
      kept-old-versions 5)
;; Set up a file autosave directory
(defvar my-auto-save-folder (concat "~/.emacs.d/auto-save/")); folder for auto-saves
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save/.saves-"); set prefix for auto-saves 
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t))); location for all auto-save files
(setq tramp-auto-save-directory my-auto-save-folder); auto-save tramp files in local directory

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; org-mode
;; org-agenda
(setq org-agenda-files '("~/project/document/org")
      org-agenda-include-diary t
      )
(global-set-key (kbd "C-c a") 'org-agenda)
;; disable pairing of < and > in org-mode src blocks
(add-hook 'org-mode-hook
          (lambda ()
            (modify-syntax-entry ?< ".")
            (modify-syntax-entry ?> ".")))

;; set checkbox
(add-hook 'org-mode-hook (lambda ()
  "Beautify Org Checkbox Symbol"
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

;; compilation 文字化け
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; windows move
(windmove-default-keybindings)

;; python mode
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
(setq python-shell-interpreter "python3")

;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; i-search lazy-count
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;;; C++ IDE
(which-key-mode)
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
(defun chromium-c++-mode-hook ()
  (setq c-basic-offset 2)     ;; indentation width
  (setq indent-tabs-mode nil) ;;
  (c-set-offset 'case-label 2);; indent case labels by 2 spaces
  (c-set-offset 'innamespace 0))

(add-hook 'c++-mode-hook 'chromium-c++-mode-hook)

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details

(require 'helm)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
;(define-key helm-map (kbd "C-c >") 'helm-toggle-truncate-line)
(helm-mode 1)


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      )  ;; clangd is fast

;; org-mode src blocks
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t) (C . t) (shell . t)))

;; display buffer at bottom
(setq display-buffer-alist
      '(("\\*xref\\*" . ((display-buffer-at-bottom display-buffer-pop-up-window)))
	("\\*grep\\*" . ((display-buffer-at-bottom display-buffer-pop-up-window)))
        ("\\*Org Agenda\\*" . ((display-buffer-at-bottom)))))

;; set time clock

(setq display-time-format "%a %m/%d, %H:%M:%S")
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(display-time-mode 1)

(global-set-key (kbd "M-SPC") 'set-mark-command)

;; font
;; (defun my-better-hybird-font ()
;;   ;; 设置混合字体
;;   (dolist (param '(
;;                    (font . "Noto Sans JP")
;;     (add-to-list 'default-frame-alist param)
;;                    ))
;;     (add-to-list 'initial-frame-alist param)
;;     ))

;; (my-better-hybird-font)

;; restart with saved buffer
(desktop-save-mode 1)
;; (setq desktop-load-restore-eager 0)


(put 'magit-clean 'disabled nil)
