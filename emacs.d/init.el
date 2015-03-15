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
		      smartparens
		      cider
		      exec-path-from-shell
		      moe-theme
		      magit))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(smartparens-global-mode t)

(moe-dark)
