(require 'package)
(add-to-list 'package-archives
	     '("melpha-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/elisp/rvm.el")

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
		     projectile
		     paredit
		     cider
		     exec-path-from-shell
		     moe-theme
		     magit
		     company
		     scala-mode
		     sbt-mode
		     ensime
		     haskell-mode
		     rainbow-delimiters
		     impatient-mode
		     dockerfile-mode
		     web-mode
		     helm
		     helm-projectile
		     ruby-refactor
		     flycheck
		     robe
		     rinari
		     rspec-mode
		     ruby-end))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p))) 

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (global-set-key (kbd "M-3") '(lambda()(interactive)(insert "#"))))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'ruby-mode-hook 'ruby-hooks)

(defun ruby-hooks ()
  "Ruby plugins."
  (ruby-refactor-mode 1)
  (robe-mode 1)
  (rvm-activate-corresponding-ruby)
  (rinari-minor-mode 1)
  (rspec-mode 1)
  (ruby-end-mode 1))

(require 'rvm)
(rvm-use-default)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ruby-end mongo yaml-mode web-mode scala-mode2 rvm ruby-refactor robe rainbow-delimiters paredit moe-theme magit impatient-mode helm-projectile haskell-mode flycheck exec-path-from-shell ensime dockerfile-mode cider)))
 '(terraform-indent-level 4))

(fset 'insertPound "#")
(global-set-key (kbd "M-3") 'insertPound)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(setq cider-repl-use-clojure-font-lock t)

(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'shell-mode-hook
      	  'ansi-color-for-comint-mode-on)

(global-flycheck-mode)

(require 'moe-theme)
(moe-dark)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
