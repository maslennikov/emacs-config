;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-php-module-init ()

  (ecfg-install php-mode
    (autoload 'php-mode "php-mode" "Editing PHP" t)
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
    (add-hook 'php-mode-hook 'ecfg--php-hook)
    )
)
;;;###autoload (ecfg-auto-module "\\.php$" php)

(defun ecfg--php-hook ()

  )
