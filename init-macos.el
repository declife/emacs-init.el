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
   '(avy company flycheck magit mozc vterm yasnippet vertico orderless consult
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

;;; General settings
(setq inhibit-startup-message t)       ;; Disable the startup message
(tool-bar-mode -1)                     ;; Disable the toolbar
(menu-bar-mode -1)                     ;; Disable the menu bar
(scroll-bar-mode -1)                   ;; Disable the scroll bar
(windmove-default-keybindings)         ;; windows move
(global-display-line-numbers-mode t)   ;; Show line numbers
(setq column-number-mode t)   ;; Show colume numbers
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Better buffer management
(global-set-key (kbd "M-SPC") 'set-mark-command)

(setq-default indent-tabs-mode nil)    ;;tab key indent
(setq-default tab-width 4)             ;;tab key width

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
(desktop-save-mode 1)
;; (setq desktop-load-restore-eager 0)

;; compilation 文字化け
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;; i-search lazy-count
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

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

;;; magit mode
;(setq magit-ediff-dwim-show-on-hunks nil)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)


;;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;;; org-mode
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

;; org-mode src blocks
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t) (C . t) (shell . t)))

;; set checkbox
(add-hook 'org-mode-hook (lambda ()
  "Beautify Org Checkbox Symbol"
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

;; python mode
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
(setq python-shell-interpreter "python3")

;;; C++ IDE
(which-key-mode)
(defun chromium-c++-mode-hook ()
  (setq c-basic-offset 2)     ;; indentation width
  (setq indent-tabs-mode nil) ;;
  (c-set-offset 'case-label 2);; indent case labels by 2 spaces
  (c-set-offset 'innamespace 0))

(add-hook 'c++-mode-hook 'chromium-c++-mode-hook)

;;;search and navigatio consult && minibuffer vertico

;; consult
(use-package consult
  :ensure t
  :bind
  (;; 常用命令快捷键绑定
   ("C-s" . consult-line)             ;; 在当前 buffer 内搜索文本
   ("C-M-l" . consult-imenu)          ;; 跳转到函数/标记
   ("M-y" . consult-yank-pop)         ;; 更好的 kill-ring 浏览器
   ("C-c h" . consult-history)        ;; minibuffer 历史记录
   ("C-c m" . consult-mode-command)   ;; 当前 major-mode 的命令列表
   ("C-c g" . consult-ripgrep)        ;; ripgrep 搜索（需安装 ripgrep）
   )
  :hook
  (completion-list-mode . consult-preview-at-point-mode) ;; 预览支持
  :init
  ;; 如果你使用 project.el（Emacs 自带的项目管理），设置 consult 的项目 root 获取方式：
  (setq consult-project-root-function
        (lambda ()
          (when-let* (project (project-current))
            (car (project-roots project)))))
  :custom
  (consult-preview-key "M-.") ;; 手动触发预览，可以防止大项目卡顿
)

;; Enable Vertico.
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

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
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(setq exec-path (append exec-path '("/usr/local/bin")))


;;(put 'magit-clean 'disabled nil)
