;; -*- lexical-binding: t -*-

(defun ecfg-c-module-init ()
  ;; it is handled in cpp-module
  ;; (add-hook 'c-mode-hook 'ecfg--c-hook)
)

(defun ecfg--c-hook ()
  (subword-mode)
  (c-set-style "k&r")
  (c-set-offset 'arglist-close 0)
  (setq c-basic-offset 4))
