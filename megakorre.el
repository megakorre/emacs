(display-time)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/")

;; imports
(require 'pk-theme)
(require 'package)
(require 'util)
(progn (require 'ido) (ido-mode 1))

(pending-delete-mode 1)
(setq

 make-backup-files           nil
 ido-enable-flex-matching    t
 ido-everywhere              t

 mac-option-modifier         nil
 mac-command-modifier        'meta
 x-select-enable-clipboard   t

 inferior-lisp-program       "sbcl"
 ring-bell-function          #'ignore
 package-archives            '(("gnu" . "http://elpa.gnu.org/packages/")
		              ("marmalade" . "http://marmalade-repo.org/packages/")
		              ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun save-macro (name)
  "save a macro. Take a name as argument
indent-regionnd save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(defun back-and-tab ()
  (interactive)
  (setq last-kbd-macro))

(el-get-packages
 '(:name popup :type github :pkgname "auto-complete/popup-el")
 '(:name ido-ubiquitous :type elpa :after (ido-ubiquitous))

 '(:name Enhanced-Ruby-Mode
	 :type github
	 :pkgname "jacott/Enhanced-Ruby-Mode"
	 :features ruby-mode)

 (github-package 'magit             "magit/magit")
 (github-package 'dash              "magnars/dash.el")
 (github-package 'loop              "Wilfred/loop.el")
 (github-package 's                 "magnars/s.el")

 (github-package 'puggle-utils      "PugglePay/puggle-emacs-utils")
 (github-package 'spork-and-nailgun "PugglePay/spork-and-nailgun.el")
 (github-package 'rspec-mode        "PugglePay/rspec-mode")

 (github-package 'yaml-mode         "yoshiki/yaml-mode")
 (github-package 'ace-jump-mode     "winterTTr/ace-jump-mode")
 (github-package 'multiple-cursors  "emacsmirror/multiple-cursors")
 (github-package 'expand-region     "magnars/expand-region.el")


 (melpa-package 'find-file-in-project)
 (melpa-package 'auto-complete)
 (melpa-package 'clojure-mode)
 (melpa-package 'paredit)
 (melpa-package 'nrepl)
 (melpa-package 'rainbow-mode)
 (melpa-package 'clojure-test-mode)
 (melpa-package 'ack-and-a-half)
 (melpa-package 'rvm))

;; keybindings
(global-set-key (kbd "C-x C-l")		'sang-start-all)
(global-set-key (kbd "C-x C-p")		'ack-and-a-half)
(global-set-key (kbd "C-x C-s")		'force-save-buffer)
(global-set-key (kbd "C-x g")		'magit-status)
(global-set-key [C-tab]			'nrepl-indent-and-complete-symbol)
(global-set-key (kbd "C-x f")		'find-file-in-project)
(global-set-key (kbd "C-S-c C-S-c")	'mc/edit-lines)
(global-set-key (kbd "C-x C-i")		'esk-indent-buffer)

(global-set-key (kbd "C-.")		'complete-symbol)
(global-set-key (kbd "C-S-c C-S-c")	'mc/edit-lines)
(global-set-key (kbd "C-:")		'mc/mark-next-like-this)
(global-set-key (kbd "C-;")		'mc/mark-previous-like-this)

(global-set-key (kbd "C-v") 'er/expand-region)
(global-set-key (kbd "C-f") 'kill-whole-line)
(global-set-key (kbd "C-*") 'ace-jump-word-mode)

;; hooks
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'ruby-mode-hook              'setup-ruby-stuff)
(add-hook 'before-save-hook            'delete-trailing-whitespace)
(add-hook 'clojure-mode-hook           'setup-lisp-stuff)
(add-hook 'emacs-lisp-mode-hook        'setup-lisp-stuff)
(add-hook 'lisp-mode-hook              'setup-lisp-stuff)

(defalias 'yes-or-no-p 'y-or-n-p)

;; remove menue stuff
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; stubb functions :)
(defun hl-line-mode  (&rest varargs))
;(defun inf-ruby-keys (&rest varargs))

(defun setup-ruby-stuff ()
  (auto-complete)
  (rspec-mode)
  (auto-complete-mode))

(defvar tem-password "kevin.jalaho@bisfront.se")

(defun setup-lisp-stuff ()
  (setq nrepl-hide-special-buffers t)
  (auto-complete)
  (auto-complete)
  (auto-complete-mode)
  (paredit-mode)
  (clojure-test-mode))

(set-face-attribute 'default nil
                    :height 150
                    :weight 'normal)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash")) ad-do-it))

(ad-activate 'rspec-compile)

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(provide 'megakorre)
