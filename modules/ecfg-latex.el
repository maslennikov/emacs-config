;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-latex-module-init ()
  (add-hook 'tex-mode-hook 'ecfg--latex-hook))
;;;###autoload (ecfg-auto-module "\\.tex$" latex)

(defun ecfg--latex-hook ()
  (turn-on-auto-fill)
  (setq tex-close-quote ">>")
  (setq tex-open-quote "<<")
  (setq tex-suscript-height-ratio 0.85))
