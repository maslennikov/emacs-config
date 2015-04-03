;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-sgml-module-init ()
  (ecfg-install zencoding-mode)
  (add-hook 'sgml-mode-hook 'ecfg--sgml-hook))
;;;###autoload (ecfg-auto-module "\\.\\(x\\|ht\\|xht\\)ml$" sgml)

;; sgml - general markup mode
(defun ecfg--sgml-hook ()
  (zencoding-mode 1)
  (setq sgml-basic-offset 2))
