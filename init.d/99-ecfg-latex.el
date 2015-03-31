;; -*- lexical-binding: t -*-

(defun ecfg-latex-module-init ()
  (add-hook 'tex-mode-hook 'ecfg--latex-hook))

(defun ecfg--latex-hook ()
  (turn-on-auto-fill)
  (setq tex-close-quote ">>")
  (setq tex-open-quote "<<")
  (setq tex-suscript-height-ratio 0.85))
