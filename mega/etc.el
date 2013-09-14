(set-default 'cursor-type 'box)
(setq
 ring-bell-function #'ignore
 cursor-type 'bar
 make-backup-files           nil
 ido-enable-flex-matching    t
 ido-everywhere              t
 nrepl-hide-special-buffers t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; remove menu stuff
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(set-face-attribute 'default nil :height 130 :weight 'normal)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.1)
 '(ac-ignore-case nil)
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("96498c4437f0a3231e4585ae577b254c396e0e3c6c471ff1b1d37eafed565f2a" "3c1f0e1b78f0ecaea0468507cc3b15be6e1525b4b285e10d2e9e3cdfc0e4adf0" default)))
 '(global-auto-revert-mode t)
 '(rspec-spec-command "smart_spec")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-rake-flag nil)
 '(rspec-use-rvm t)
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(ruby-extra-keywords (quote ("protected" "private")))
 '(ruby-hanging-indent-level 2))

(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'ido)
(ido-mode 1)
