;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-jade-module-init ()
  (ecfg-install jade-mode
   (autoload 'stylus-mode "stylus-mode" nil t)
   (autoload 'jade-mode "jade-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
   (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
   (add-hook 'jade-mode-hook 'ecfg--jade-hook))
  )
;;;###autoload (ecfg-auto-module "\\.styl$" jade)
;;;###autoload (ecfg-auto-module "\\.jade$" jade)

(defun ecfg--jade-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq comment-start "//-"))
