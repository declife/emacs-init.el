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
   '("0517759e6b71f4ad76d8d38b69c51a5c2f7196675d202e3c2507124980c3c2a3" "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9" "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757" "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" default))
 '(org-agenda-files '("~/project/document/alpine.org"))
 '(package-selected-packages
   '(magit irony-eldoc company irony grip-mode pandoc-mode gruvbox-theme))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; == irony-mode ==
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; org-mode

(setq org-agenda-files '("~/project/document/org")
      org-agenda-include-diary t
      )
(global-set-key (kbd "C-c a") 'org-agenda)


;; disable backup files
(setq make-backup-files nil)

;; windows move
(windmove-default-keybindings)

;; chromium mode
;(setq-default chrome-root "~/project/pana_rse/src/")
;(add-to-list 'load-path (concat chrome-root "tools/emacs"))
;(require 'trybot)

;;; End of init.el







