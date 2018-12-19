(require 'package)
(add-to-list 'package-archives
	     '("melpha-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))

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
		     smartparens
		     docker
		     ssh-config-mode
		     rbenv
		     projectile-rails
		     easy-hugo
		     yaml-mode
		     markdown-mode
		     minitest
		     js2-mode
		     prettier-js
		     add-node-modules-path
		     alchemist))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p))) 

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (global-set-key (kbd "M-3") '(lambda()(interactive)(insert "#"))))

(require 'smartparens-config)

(define-key smartparens-mode-map (kbd "C->") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M->") 'sp-backward-barf-sexp)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'ruby-mode-hook 'ruby-hooks)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'elixir-mode-hook #'elixir-hooks)

(defun elixir-hooks ()
  "Elixir plugins"
  (alchemist-mode 1))

(defun ruby-hooks ()
  "Ruby plugins."
  (ruby-refactor-mode 1)
  (robe-mode 1)
  (rinari-minor-mode 1)
  (rspec-mode 1)
  (smartparens-mode 1)
  (rainbow-delimiters-mode 1)
  (hs-minor-mode 1)
  (setq-local company-dabbrev-downcase nil))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (smartparens-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (add-node-modules-path minitest jsx-mode ruby-end mongo yaml-mode web-mode scala-mode2 ruby-refactor robe rainbow-delimiters paredit moe-theme magit impatient-mode helm-projectile haskell-mode flycheck exec-path-from-shell ensime dockerfile-mode cider)))
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

(projectile-mode +1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-rails-global-mode)

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

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


(require 'moe-theme)
(moe-dark)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; (docker-global-mode)
(delete-selection-mode 1)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(require 'prettier-js)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))
(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))

(eval-after-load 'js2-mode
  '(progn
     (add-hook 'js2-mode-hook #'add-node-modules-path)
     (add-hook 'js2-mode-hook #'prettier-js-mode)))

(setq prettier-js-command  "prettier-eslint")

(defun js2-mode-config ()
  "Hooks for js2 mode."
  (setq js2-basic-offset 2)
  (smartparens-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'js2-mode-hook 'js2-mode-config)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
;; for byebug/pry in rspec-mode
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
