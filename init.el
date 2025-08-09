;;; Start of init.el  -*- lexical-binding: t; -*-

(cond
 ((eq system-type 'darwin) ;; macOS
  (load (expand-file-name "init-macos.el" user-emacs-directory)))

 ((eq system-type 'gnu/linux) ;; Linux
  (load (expand-file-name "init-linux.el" user-emacs-directory)))

 ((eq system-type 'windows-nt) ;; Windows
  (load (expand-file-name "init-windows.el" user-emacs-directory)))
 
 (t
  (message "Unknown system type: %s" system-type)))
