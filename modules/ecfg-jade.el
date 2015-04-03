;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-jade-module-init ()
  (ecfg-install jade-mode
   (add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
   (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
   (add-hook 'jade-mode-hook 'ecfg--jade-hook))
  ;; (autoload 'stylus-mode "stylus-mode" "load stylus mode" t)
  ;; (autoload 'jade-mode "jade-mode" "load jade mode" t)
  )
;;;###autoload (ecfg-auto-module "\\.styl$" jade)
;;;###autoload (ecfg-auto-module "\\.jade$" jade)

(defun ecfg--jade-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq comment-start "//-"))
