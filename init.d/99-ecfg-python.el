;; -*- lexical-binding: t -*-

(defun ecfg-python-module-init ()
  (add-hook 'python-mode-hook 'ecfg--py-hook))

(defun ecfg--py-hook ()
  ;;specifying underscore as a member of symbol class instead of word class
  ;; (modify-syntax-entry ?_ ".")
  (subword-mode)
  (add-hook 'local-write-file-hooks 'ecfg-end-buffer-with-blank-line))
