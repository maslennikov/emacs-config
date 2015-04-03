;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-cmake-module-init ()
  (ecfg-install cmake-mode
   (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
   (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
   (add-hook 'cmake-mode-hook 'ecfg--cmake-hook)))

;;;###autoload (ecfg-auto-module "CMakeLists\\.txt$" cmake)
;;;###autoload (ecfg-auto-module "\\.cmake$" cmake)

(defun ecfg--cmake-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq cmake-tab-width 4)
  ;; navigating through the subwords, subword-mode isn't working
  (modify-syntax-entry ?_  "_" cmake-mode-syntax-table)
  )
