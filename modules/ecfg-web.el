;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-web-module-init ()

  (ecfg-install web-mode
   (autoload 'web-mode "web-mode" nil t)

   (add-to-list 'auto-mode-alist '("\\.\\(xml\\|x?html?\\)\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

   (ecfg-install emmet-mode
    (autoload 'emmet-mode "emmet-mode" nil t)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode))

   (add-hook 'web-mode-hook 'ecfg--web-mode-hook))
  )
;;;###autoload (ecfg-auto-module "\\.\\(xml\\|x?html?\\)\\'" web)
;;;###autoload(ecfg-auto-module "\\.phtml\\'" web)
;;;###autoload(ecfg-auto-module "\\.tpl\\.php\\'" web)
;;;###autoload(ecfg-auto-module "\\.[agj]sp\\'" web)
;;;###autoload(ecfg-auto-module "\\.as[cp]x\\'" web)
;;;###autoload(ecfg-auto-module "\\.erb\\'" web)
;;;###autoload(ecfg-auto-module "\\.mustache\\'" web)
;;;###autoload(ecfg-auto-module "\\.djhtml\\'" web)
;;;###autoload(ecfg-auto-module "\\.ejs\\'" web)

(defun ecfg--web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t))
