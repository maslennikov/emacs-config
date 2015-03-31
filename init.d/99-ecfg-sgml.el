;; -*- lexical-binding: t -*-

(defun ecfg-sgml-module-init ()
  (el-get-bundle zencoding-mode)
  (add-hook 'sgml-mode-hook 'ecfg--sgml-hook))

;; sgml - general markup mode
(defun ecfg--sgml-hook ()
  (zencoding-mode 1)
  (setq sgml-basic-offset 2))
