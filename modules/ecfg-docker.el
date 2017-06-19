;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-docker-module-init ()
  (ecfg-install dockerfile-mode
   (autoload 'dockerfile-mode "dockerfile-mode" "Editing Dockerfile" t)
   (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
   (add-to-list 'auto-mode-alist '("\\.docker$" . dockerfile-mode))
))

;;;###autoload (ecfg-auto-module "Dockerfile" docker)
;;;###autoload (ecfg-auto-module "\\.docker$" docker)
