
;;Set up package management
(require 'package)

;;; code:

;;package
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Automatically install use-package for easier package management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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

;; Additional customizations can go here

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-light-hard))
 '(custom-safe-themes
   '("75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3" "5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d" "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" "0517759e6b71f4ad76d8d38b69c51a5c2f7196675d202e3c2507124980c3c2a3" "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757" "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9" default))
 '(package-selected-packages '(flycheck pandoc grip-mode markdown-mode gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :init (setq markdown-command "pandoc"))

(setq markdown-command "pandoc")
(setq markdown-fontify-code-blocks-natively t)


;;; grip-mode

;; Or start grip when opening a markdown/org buffer
(add-hook 'markdown-mode-hook #'grip-mode)
(add-hook 'org-mode-hook #'grip-mode)

;; Use embedded webkit to preview
;; This requires GNU/Emacs version >= 26 and built with the `--with-xwidgets` option.
;; (setq grip-preview-use-webkit t)

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  )

;; Encode
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; font
;; Set fonts for Japanese and Chinese text
(set-fontset-font "fontset-default" 'japanese-jisx0208 "MS Gothic")
(set-fontset-font "fontset-default" 'chinese-gb2312 "SimSun")


;;(provide 'init)
;;; init.el ends here
