;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-yaml-module-init ()
  (ecfg-install yaml-mode
    (autoload 'yaml-mode "yaml-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))))
;;;###autoload (ecfg-auto-module "\\.ya?ml\\'" yaml)
