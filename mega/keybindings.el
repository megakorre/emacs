(setq
 mac-option-modifier nil
 mac-command-modifier 'meta
 x-select-enable-clipboard t)

(mega/keys
 (:tap "qq" m-indent-buffer)
 (:tap "xx" ace-jump-word-mode)
 (:tap "åå" kill-whole-line)
 (:tap "äa" rspec-verify-all)
 (:tap "äv" rspexc-verify)
 (:tap "äc" rspec-verify-continue)
 (:tap "äs" kill-whole-line)
 (:tap "ät" rspec-toggle-spec-and-target)

 ("C--" evil-join)
 ("C-x C-l" sang-start-all)
 ("C-x C-p" ack-and-a-half)
 ("C-x C-s" force-save-buffer)
 ("C-x C-j" gnus)
 ("C-x g" magit-status)
 ("C-x f" fiplr-find-file)
 ("C-S-c C-S-c" mc/edit-lines)
 ("C-x C-i" esk-indent-buffer)
 ("C-." complete-symbol)

 ("RET" newline-and-indent)

 ("C-S-c C-S-c" mc/edit-lines)
 ("C-:" mc/mark-next-like-this)
 ("C-;" mc/mark-previous-like-this)
 (mc/keymap "C-v" nil)

 ("C-v" er/expand-region)
 ("C-f" kill-whole-line)
 ("C-t" ace-jump-word-mode)
 ("C-l" insert-and-indent-before)
 ("C-ö" insert-and-indent-after)

 (git-commit-mode-map "C-x i" pivotal-make-ref)
 ("C-ä" pivotal-make-ref)

 ("C-a" move-to-begining-of-code)
 ("C-b" ido-switch-buffer)
 ("C-1" delete-other-windows)
 ("C-0" delete-window))
