;; -*- lexical-binding: t -*-

(defun ecfg-cmake-module-init ()
  (el-get-bundle cmake-mode
   :before (autoload 'cmake-mode "cmake-mode" "Editing cmake files" t)
   :after
   (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
   (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
   (add-hook 'cmake-mode-hook 'ecfg--cmake-hook)
                 )
)

(defun ecfg--cmake-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq cmake-tab-width 4)
  ;; navigating through the subwords, subword-mode isn't working
  (modify-syntax-entry ?_  "_" cmake-mode-syntax-table)
  )
