(require 'key-chord)
(key-chord-mode 1)

(setq
  mac-option-modifier         nil
  mac-command-modifier        'meta
  x-select-enable-clipboard   t)

(key-chord-define-global "qq" 'm-indent-buffer)
(key-chord-define-global "xx" 'ace-jump-word-mode)
(key-chord-define-global "åå" 'kill-whole-line)

(key-chord-define-global "äa" 'rspec-verify-all)
(key-chord-define-global "äv" 'rspec-verify)
(key-chord-define-global "äc" 'rspec-verify-continue)
(key-chord-define-global "äs" 'kill-whole-line)
(key-chord-define-global "ät" 'rspec-toggle-spec-and-target)

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

 "C-t"		'ace-jump-word-mode

 "C-l"          'insert-and-indent-before
 "C-ö"          'insert-and-indent-after
 "C-ä"          'pivotal-make-ref

 "C-a"          'move-to-begining-of-code
 "C-b"          'ido-switch-buffer

 "C-1"          'delete-other-windows
 "C-0"          'delete-window)
