;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-markdown-module-init ()
  (ecfg-install markdown-mode
    (autoload 'markdown-mode "markdown-mode" nil t)
    (add-to-list 'auto-mode-alist
                 '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
;;;###autoload (ecfg-auto-module "\\.\\(md\\|mdown\\|markdown\\)\\'" markdown)
