(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpha-stable" . "http://melpa-stable.milkbox.net/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
		      projectile
		      smartparens
		      cider
		      exec-path-from-shell))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

