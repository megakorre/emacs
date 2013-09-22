;;
(defalias 'first 'car)
(defalias 'rest  'cdr)


(defun force-save-buffer ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (delete-trailing-whitespace)
  (save-buffer))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (setq my-packages (mapcar 'el-get-source-name el-get-sources))
  (el-get 'sync my-packages))

(defun el-get-init ()
  (if (require 'el-get nil t)
      (sync-packages)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)
       (setq el-get-verbose t)
       (sync-packages)))))

(defun el-get-packages (&rest packages)
  (package-initialize)
  (setq el-get-sources packages)
  (el-get-init)
  (package-initialize))

(defun in-modes? (modes)
  (-contains? modes major-mode))

(defun move-to-begining-of-code ()
  (interactive)
  (move-beginning-of-line 1)
  (indent-according-to-mode))

(defun insert-and-indent-after ()
  (interactive)
  (message "inserting line after")
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun insert-and-indent-before ()
  (interactive)
  (message "inserting line before")
  (previous-line)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun m-indent-buffer ()
  (interactive)
  (message "indenting all buffer")
  (puggle-indent-buffer))

(defun evil-join ()
  (interactive)
  (delete-indentation -1))

(defun slurp (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun iterate (f x)
  (let ((n (funcall f x)))
    (if n (cons x (iterate f n)))))

(defun range (start &optional end)
  (let ((start (if end start 0))
	(end   (if end end start)))
    (iterate
     (lambda (n)
       (if (eq n end) nil (1+ n)))
     start)))

(defun scan (f col &optional seed)
  (cond
   ((not col)
    (list seed))
   ((not seed)
    (scan f (cdr col) (car col)))
   (t (cons seed
	    (scan f (cdr col) (funcall f seed (car col)))))))

(defun combinations (l n)
  (cond
   ((eq n 0) nil)
   ((eq n 1) (-map 'list l))
   (t (-mapcat
       (lambda (item)
	 (-map (lambda (s) (cons item s))
	       (combinations (remove item l) (- n 1))))
       l))))



(require 'key-chord)
(key-chord-mode 1)

(require 'dash-functional)

(defun mega/dispatch-key (expression)
  (cond
   ;; key-chord
   ((eq (first expression) :tap)
    `(key-chord-define-global ,(second expression) (quote ,(third expression))))

   ;; keymap
   ((symbolp (first expression))
    `(define-key ,(first expression) (kbd ,(second expression)) (quote ,(third expression))))

   ;; kbd
   ((stringp (first expression))
    `(global-set-key (kbd ,(first expression)) (quote ,(second expression))))

   ;; raw
   (t
    `(global-set-key ,(first expression) (quote ,(second expression))))))

(defmacro mega/keys (&rest args)
  (cons 'progn (-map 'mega/dispatch-key args)))
