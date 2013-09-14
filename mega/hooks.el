(setq
  lisp-modes '(clojure-mode
	      emacs-lisp-mode
	      inferior-emacs-lisp-mode
	      lisp-mode
	      IELM
	      Emacs-Lisp)

 ruby-modes '(ruby-mode EnhRuby))

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash")) ad-do-it))

(ad-activate 'rspec-compile)
