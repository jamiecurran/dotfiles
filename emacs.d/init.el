(require 'package)
(add-to-list 'package-archives
	     '("melpha-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
		      projectile
		      paredit
		      cider
		      exec-path-from-shell
		      moe-theme
		      magit))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'moe-theme)
(moe-dark)
