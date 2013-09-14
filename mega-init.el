(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/")

(load "mega/boot-utils")
(load "mega/packages")
(load "mega/el-get")

(mega/boot-el-get)

(load "mega/style")
(load "mega/utils")
(load "mega/keybindings")
(load "mega/etc")
(load "mega/hooks")

(require 'pivotol)
