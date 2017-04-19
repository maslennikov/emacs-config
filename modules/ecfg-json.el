;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-json-module-init ()
  (ecfg-install json-mode
   ;; json-mode intelligently registers its auto-mode alist settings, so let it be
   (ecfg-with-local-autoloads

    (add-hook 'json-mode-hook 'ecfg--json-hook))))

;;;###autoload (ecfg-auto-module "\\.json$" json)

(defun ecfg--json-hook ()
  (setq js-indent-level 2)
  )
