;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-css-module-init ()

  ;; basic css settings
  (add-hook 'css-mode-hook 'ecfg--css-hook)

  (ecfg-install emmet-mode
    (autoload 'emmet-mode "emmet-mode" nil t)
    (add-hook 'css-mode-hook 'emmet-mode))

  ;; supporting sass scss syntax
  (ecfg-install scss-mode
   (autoload 'scss-mode "scss-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
   (add-hook 'scss-mode-hook 'ecfg--scss-hook))

  ;; utilizing helm in navigating through stylesheet properties
  (ecfg--setup-helm-css-scss)
  )
;;;###autoload (ecfg-auto-module "\\.s?css$" css)


(defun ecfg--css-hook ()
  (setq css-indent-offset 2))

(defun ecfg--scss-hook ()
  ;; the css-mode-hook will be already executed by this moment
  (setq scss-compile-at-save nil))


(defun ecfg--setup-helm-css-scss ()
  (ecfg-install helm-css-scss
   (ecfg-with-local-autoloads

    ;; Allow comment inserting depth at each end of a brace
    (setq helm-css-scss-insert-close-comment-depth 2)
    ;; Split direction. 'split-window-vertically or 'split-window-horizontally
    (setq helm-css-scss-split-direction 'split-window-vertically)

    ;; Set local keybind map for css-mode / scss-mode / less-css-mode
    (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
      (add-hook $hook (lambda ()
        (local-set-key (kbd "s-i") 'helm-css-scss)
        (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

    ;; (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
  )))
