;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-coffee-module-init ()
  (ecfg-install coffee-mode
   (add-hook 'coffee-mode-hook 'ecfg--coffee-hook))
)
;;;###autoload (ecfg-auto-module "\\.coffee$" coffee)
;;;###autoload (ecfg-auto-module "Cakefile" coffee)

(defun ecfg--coffee-hook ()
  )
