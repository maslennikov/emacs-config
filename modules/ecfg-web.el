;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-web-module-init ()

  (ecfg-install web-mode
   (autoload 'web-mode "web-mode" nil t)

   (add-to-list 'auto-mode-alist '("\\.\\(xml\\|x?html?\\)\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.\\(tpl\\|tpl\\.php\\)\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
   ;; exclude mustashe since it's markup-agnostic and doesn't mean web markup
   ;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

   (ecfg-install emmet-mode
    (autoload 'emmet-mode "emmet-mode" nil t)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode))

   (add-hook 'web-mode-hook 'ecfg--web-mode-hook)
   (add-hook 'emmet-mode-hook 'ecfg--emmet-mode-hook)
  ))
;;;###autoload (ecfg-auto-module "\\.\\(xml\\|x?html?\\)\\'" web)
;;;###autoload(ecfg-auto-module "\\.phtml\\'" web)
;;;###autoload(ecfg-auto-module "\\.\\(tpl\\|tpl\\.php\\)\\'" web)
;;;###autoload(ecfg-auto-module "\\.[agj]sp\\'" web)
;;;###autoload(ecfg-auto-module "\\.as[cp]x\\'" web)
;;;###autoload(ecfg-auto-module "\\.erb\\'" web)
;;;###autoload(ecfg-auto-module "\\.djhtml\\'" web)
;;;###autoload(ecfg-auto-module "\\.ejs\\'" web)
;;;###autoload(ecfg-auto-module "\\.twig\\'" web)
;;;###autoload(ecfg-auto-module "\\.jsx$" web)
;; ;;;###autoload(ecfg-auto-module "\\.mustache\\'" web)

(defun ecfg--web-mode-hook ()
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-current-column-highlight t
   web-mode-enable-current-element-highlight t
   ; we will teach electric-pairs to do this
   web-mode-enable-auto-pairing nil
   web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
   )

  (setq-local
   ; not appending to electric-pair-text-pairs
   electric-pair-pairs (append electric-pair-pairs '((?% . ?%))))

  (if (equalp (file-name-extension (or (buffer-file-name) (buffer-name))) "jsx")
      (progn
        (ecfg-javascript-module-init)
        (js2-minor-mode)
        ))

  (define-key web-mode-map (kbd "M-p") 'web-mode-element-previous)
  (define-key web-mode-map (kbd "M-n") 'web-mode-element-next)
  (define-key web-mode-map (kbd "M-a") 'web-mode-element-beginning)
  (define-key web-mode-map (kbd "M-e") 'web-mode-element-end)

  )

(defun ecfg--emmet-mode-hook ()
  )
