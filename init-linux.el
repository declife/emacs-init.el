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
   '(company consult doom-modeline flycheck gt magit marginalia
             modus-themes orderless projectile vertico vterm
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

;;; themes
(load-theme 'zenburn t)

;;(load-theme 'modus-operandi-tinted t)

;; magit mode
;(setq magit-ediff-dwim-show-on-hunks nil)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)


;;; General settings
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

;; display buffer at bottom
(setq display-buffer-alist
      '(("\\*xref\\*" . ((display-buffer-at-bottom display-buffer-pop-up-window)))
	("\\*grep\\*" . ((display-buffer-at-bottom display-buffer-pop-up-window)))
        ("\\*Org Agenda\\*" . ((display-buffer-at-bottom)))))

;; set time clock

(setq display-time-format "%H:%M:%S, %a %m/%d")
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; (defun display-time-bottom-right ()
;;   (and (equal (cddr (window-pixel-edges))
;;               (cddr (window-pixel-edges (frame-root-window))))
;;        '(#(" " 0 1 (display (space :align-to (- right 20))))
;;          display-time-string)))


;; (setq global-mode-string '(:eval (display-time-bottom-right)))

;;; desktop save mode
;; restart with saved buffer
(desktop-save-mode 1)

;; optimize desktop performance
(setq desktop-restore-eager 5          ;; immediately restore the previous 5 buffers
      desktop-lazy-idle-delay 1        ;; load other buffers after 1s
      desktop-lazy-verbose nil         ;; not show lazy load info
      desktop-load-locked-desktop t)   ;; allow to load locked desktop

; exclude non-need buffer type
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

; exclude buffer of special name
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|"         ;; Gnus
              "^tags\\|"                 ;; tags files
              "^TAGS\\|"                 ;; TAGS files
              "\\*.*\\*"                 ;; all temporary buffers named with surrounding asterisks
              "\\)$"))


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

;; src blocks
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t) (C . t) (shell . t)))

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

(which-key-mode)

(global-set-key (kbd "C-c o") 'ff-find-other-file)
;;; C++ IDE
(defun chromium-c++-mode-hook ()
  (setq c-basic-offset 2)     ;; indentation width
  (setq indent-tabs-mode nil) ;;
  (c-set-offset 'case-label 2);; indent case labels by 2 spaces
  (c-set-offset 'innamespace 0))

(add-hook 'c++-mode-hook 'chromium-c++-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        minibuffer and search(find/grep)                                  ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable Vertico.
(use-package vertico
  :init
  (vertico-mode)  
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-count-format '("[%2s] " . "%s/%s"))
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-group-format "%s")
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config
  (set-face-attribute 'vertico-group-title nil
                      :underline t
                      :weight 'normal
                      :background nil)
  (with-eval-after-load 'zenburn-theme
    (set-face-attribute 'vertico-current nil
                        :background "#5e5e5c"
                        :extend t
                        :box '(:line-width -1 :color "#888888")
                        :underline nil))
  )
;; marginalia file information
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; consult
(use-package consult
  :ensure t
  :bind
  (;; shortcut
;;   ("C-s" . consult-line)             ;; search in current buffer
   ("C-M-l" . consult-imenu)          ;; jump to function markup
   ("M-y" . consult-yank-pop)         ;; key-ring
   ("C-c h" . consult-history)        ;; minibuffer history
   ("C-c m" . consult-mode-command)   ;; command list of current major-mode
   ("C-c f" . consult-fd)             ;; fd
   ("C-c g" . consult-ripgrep)        ;; ripgrep
   )
  :hook
  (completion-list-mode . consult-preview-at-point-mode) ;; preview support
  :init
  ;; obtain project root
  (setq consult-project-root-function
        (lambda ()
          (when-let* (project (project-current))
            (car (project-roots project)))))
  :custom
  (consult-preview-key "C-<return>") ;; preview manually M-.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          minibuffer and search(find/grep)                                ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; translate
(use-package gt
  :ensure t
  :config
  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh ja))
         :engines (list (gt-google-engine))
         :render (gt-buffer-render
                  :dislike-header t
                  )))
  (setq gt-polyglot-p t)
  ;; (setq display-buffer-alist
  ;;       '(("\\*gt-result\\*" . ((display-buffer-at-bottom display-buffer-pop-up-window)))))
  )

(global-set-key (kbd "C-c t") 'gt-translate)

;;; doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;;(setq doom-modeline-minor-modes t)
  (setq doom-modeline-vcs-max-length 20)
  ;; (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-time t)
  (setq doom-modeline-vcs t))

;;; end of init
