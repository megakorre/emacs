(display-time)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/")
(defun ruby-mode-hook ())

;; imports
(require 'pk-theme)
(require 'package)
(require 'util)
(progn (require 'ido) (ido-mode 1))

(set-default 'cursor-type 'box)
(setq

 nrepl-hide-special-buffers t

 puggle-projects
 `((:name "emacs" :path "~/src/emacs")
   (:name "puggle-emacs" :path "~/src/puggle/puggle-emacs-utils")
   (:name "spork-and-nailgun" :path "~/src/puggle/spork-and-nailgun.el")
   (:name "mantis"  :path "~/src/puggle/mantis")
   (:name "boar"    :path "~/src/puggle/boar")
   (:name "meerkat" :path "~/src/puggle/meerkat"))

 make-backup-files           nil
 ido-enable-flex-matching    t
 ido-everywhere              t

 mac-option-modifier         nil
 mac-command-modifier        'meta
 x-select-enable-clipboard   t

 cursor-type 'bar

 lisp-modes '(clojure-mode
	      emacs-lisp-mode
	      inferior-emacs-lisp-mode
	      lisp-mode
	      IELM
	      Emacs-Lisp)

 ruby-modes '(ruby-mode EnhRuby)

 inferior-lisp-program       "sbcl"
 ring-bell-function          #'ignore
 package-archives            '(("gnu" . "http://elpa.gnu.org/packages/")
			       ("marmalade" . "http://marmalade-repo.org/packages/")
			       ("melpa" . "http://melpa.milkbox.net/packages/")))

(el-get-packages
 '(:name popup :type github :pkgname "auto-complete/popup-el")
 '(:name ido-ubiquitous :type elpa :after (ido-ubiquitous))

 '(:name Enhanced-Ruby-Mode
	 :type git
	 :url "git://github.com/Jell/Enhanced-Ruby-Mode.git"
	 :load "ruby-mode.el")

 (github-package 'magit             "magit/magit")
 (github-package 'dash              "magnars/dash.el")
 (github-package 'loop              "Wilfred/loop.el")
 (github-package 's                 "magnars/s.el")
 (github-package 'zencoding-mode    "rooney/zencoding")

 (github-package 'puggle-utils      "PugglePay/puggle-emacs-utils")
 (github-package 'spork-and-nailgun "PugglePay/spork-and-nailgun.el")

 (github-package 'yaml-mode         "yoshiki/yaml-mode")
 (github-package 'ace-jump-mode     "winterTTr/ace-jump-mode")
 (github-package 'multiple-cursors  "emacsmirror/multiple-cursors")
 (github-package 'expand-region     "magnars/expand-region.el")
 (github-package 'smartparens       "Fuco1/smartparens")
 (github-package 'elixir-mode       "elixir-lang/emacs-elixir")
 (github-package 'rspec-mode        "pezra/rspec-mode")
 (github-package 'grizzl            "d11wtq/grizzl")
 (github-package 'fiplr             "d11wtq/fiplr")

 (melpa-package 'find-file-in-project)
 (melpa-package 'auto-complete)
 (melpa-package 'clojure-mode)
 (melpa-package 'paredit)
 (melpa-package 'nrepl)
 (melpa-package 'rainbow-mode)
 (melpa-package 'clojure-test-mode)
 (melpa-package 'ack-and-a-half)
 (melpa-package 'rvm))

(global-auto-complete-mode)
(add-hook 'after-change-major-mode-hook 'mode-change)
(defun mode-change ()
  (pending-delete-mode 1)
  (smartparens-mode 1)
  (sp-use-paredit-bindings)
  (show-paren-mode 1)

  (when (in-modes? lisp-modes)
    (paredit-mode)
    (clojure-test-mode)
    (smartparens-mode 0))

  (when (in-modes? ruby-modes)
    (rspec-mode)))

(global-set-key [f1] 'puggle-files-in-project)
(global-set-keys
 "RET"          'newline-and-indent
 "C-x C-l"	'sang-start-all
 "C-x C-p"	'ack-and-a-half
 "C-x C-s"	'force-save-buffer
 "C-x C-j"      'gnus
 "C-x g"	'magit-status
 "C-x f"	'fiplr-find-file
 "C-S-c C-S-c"	'mc/edit-lines
 "C-x C-i"	'esk-indent-buffer
 "C-."		'complete-symbol
 "C-S-c C-S-c"	'mc/edit-lines
 "C-:"		'mc/mark-next-like-this
 "C-;"		'mc/mark-previous-like-this
 "C-v"		'er/expand-region
 "C-f"		'kill-whole-line
 "C-t"		'ace-jump-word-mode)

(global-set-key (kbd "C-c s") 'ispell)

;; unbind for expand region to work
;; while in multiple cursors mode
(define-key mc/keymap (kbd "C-v") nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

;; remove menu stuff
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
x(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash")) ad-do-it))

(ad-activate 'rspec-compile)
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

(provide 'megakorre)
