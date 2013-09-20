(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		    ("marmalade" . "http://marmalade-repo.org/packages/")
		    ("melpa" . "http://melpa.milkbox.net/packages/"))

 mega/packages
 (list
  '(:name popup :type github :pkgname "auto-complete/popup-el")
  '(:name ido-ubiquitous :type elpa :after (ido-ubiquitous))

  '(:name Enhanced-Ruby-Mode
	  :type git
	  :url "git://github.com/Jell/Enhanced-Ruby-Mode.git"

	  :load "ruby-mode.el")
  
  `(:name "git-modes"
          :type github
          :pkgname "magit/git-modes"
          :after (progn
                   (require 'git-commit-mode)
                   (require 'git-rebase-mode)
                   (require 'gitconfig-mode)
                   (require 'gitignore-mode)
                   (require 'gitattributes-mode)))

  (github-package 'magit             "magit/magit")
  (github-package 'dash              "magnars/dash.el")
  (github-package 'loop              "Wilfred/loop.el")
  (github-package 's                 "magnars/s.el")
  (github-package 'zencoding-mode    "rooney/zencoding")
  (github-package 'maps              "megakorre/maps")
  (github-package 'pivotal           "megakorre/pivotal")

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

  (melpa-package 'web-mode)
  (melpa-package 'key-chord)
  (melpa-package 'find-file-in-project)
  (melpa-package 'auto-complete)
  (melpa-package 'clojure-mode)
  (melpa-package 'paredit)
  (melpa-package 'nrepl)
  (melpa-package 'rainbow-mode)
  (melpa-package 'clojure-test-mode)
  (melpa-package 'ack-and-a-half)
  (melpa-package 'rvm)))
