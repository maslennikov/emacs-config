;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-jade-module-init ()
  (ecfg-install jade-mode
   (autoload 'stylus-mode "stylus-mode" nil t)
   (autoload 'jade-mode "jade-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
   (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
   (add-hook 'jade-mode-hook 'ecfg--jade-hook)
   (add-hook 'stylus-mode-hook 'ecfg--stylus-hook))
  )
;;;###autoload (ecfg-auto-module "\\.styl$" jade)
;;;###autoload (ecfg-auto-module "\\.jade$" jade)

(defun ecfg--jade-hook ()
  ;; (setq tab-width 2)
  (setq comment-start "//-"))

(defun ecfg--stylus-hook ()
  ;; (setq tab-width 2)
  (setq comment-start "//"))
