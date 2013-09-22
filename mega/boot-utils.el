(defun github-package (name package)
  (list :name name
	:type 'github
	:pkgname ,package
	:after `(progn
		  (require (quote ,name)))))

(defun melpa-package (name)
  (list :name name
	:type 'elpa
	:after `(progn (require (quote ,name)))))

(defconst ruby-block-beg-re
  "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do")

(defconst ruby-non-block-do-re
  "\\(while\\|until\\|for\\|rescue\\)\\>")

(defconst ruby-indent-beg-re
  "\\(\\s *\\(class\\|module\\|def\\)\\)\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin")

(defconst ruby-modifier-beg-re
  "if\\|unless\\|while\\|until")

(defconst ruby-modifier-re
  (concat ruby-modifier-beg-re "\\|rescue"))

(defconst ruby-block-mid-re
  "then\\|else\\|elsif\\|when\\|rescue\\|ensure")

(defconst ruby-block-op-re
  "and\\|or\\|not")

(defconst ruby-block-hanging-re
  (concat ruby-modifier-beg-re "\\|" ruby-block-op-re))

(defconst ruby-block-end-re "end")
