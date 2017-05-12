;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-docker-module-init ()
  (ecfg-install dockerfile-mode
   (autoload 'dockerfile-mode "dockerfile-mode" "Editing Dockerfile" t)
))

;;;###autoload (ecfg-auto-module "Dockerfile" docker)
