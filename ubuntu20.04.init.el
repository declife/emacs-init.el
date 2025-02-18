;;; Start of init.el


;;Set up package management
(require 'package)

;;; code:

;;package
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-light-soft))
 '(custom-safe-themes
   '("0517759e6b71f4ad76d8d38b69c51a5c2f7196675d202e3c2507124980c3c2a3"
     "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9"
     "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757"
     "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176"
     default))
 '(org-agenda-files '("~/project/document/alpine.org"))
 '(package-selected-packages
   '(avy company dap-mode flycheck helm-lsp helm-xref hydra lsp-mode
	 lsp-treemacs projectile vterm which-key yasnippet)))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; magit mode
;(setq magit-ediff-dwim-show-on-hunks nil)
(setq ediff-split-window-function 'split-window-vertically)
(setq ediff-merge-split-window-function 'split-window-vertically)


;; General settings
(setq inhibit-startup-message t)       ;; Disable the startup message
(tool-bar-mode -1)                     ;; Disable the toolbar
;;(menu-bar-mode -1)                     ;; Disable the menu bar
(scroll-bar-mode -1)                   ;; Disable the scroll bar
(global-display-line-numbers-mode t)   ;; Show line numbers
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Better buffer management

;; Set up a file backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t    ;; Avoid symlinks
      version-control t      ;; Use version numbers on backups
      delete-old-versions t  ;; Delete excess backups
      kept-new-versions 20
      kept-old-versions 5)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; == irony-mode == delete
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)

;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; lsp-mode
(setq lsp-keymap-prefix "s-j")
;; org-mode
;; org-agenda
(setq org-agenda-files '("~/project/document/org")
      org-agenda-include-diary t
      )
(global-set-key (kbd "C-c a") 'org-agenda)

;; set checkbox
(add-hook 'org-mode-hook (lambda ()
  "Beautify Org Checkbox Symbol"
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

;; org-mode src blocks
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t) (C . t)))


;; compilation 文字化け
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; disable backup files
;; (setq make-backup-files nil)

;; windows move
;(windmove-default-keybindings)


(defun my-vterm-custom-buffer-name (name)
  "Open a new vterm buffer with a specified NAME."
  (interactive "sEnter buffer name: ")
  (let ((buffer (get-buffer-create (concat "*VTERM-" name "*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    (switch-to-buffer buffer)))
(global-set-key (kbd "s-t") 'my-vterm-custom-buffer-name)


;; python mode
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
(setq python-shell-interpreter "python3")

;; indent
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)


;;; C++ IDE

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details

(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(helm-mode 1)


(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))


;; (global-ede-mode t)
;;; End of init.el







