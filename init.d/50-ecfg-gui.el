;; configuring the look and feel
;;
;; -*- lexical-binding: t -*-

(defun ecfg-gui-module-init ()
  "Entry function for ecfg init system."

  (ecfg--setup-basic-gui)
  (ecfg--setup-sml-modeline)
  (ecfg--setup-diminish)
  (ecfg--setup-color-theme))


(defun ecfg--setup-basic-gui ()
  (setq-default inhibit-startup-screen t)
  (show-paren-mode t)
  (global-hl-line-mode t)
  (column-number-mode t))

(defun ecfg--setup-sml-modeline ()
  (ecfg-install sml-modeline
   (require 'sml-modeline)
   (setq sml-modeline-borders '("[" . "]"))
   (setq sml-modeline-len 14)
   (sml-modeline-mode t)))

(defun ecfg--setup-diminish ()
  (ecfg-install
   diminish
   (eval-after-load "abbrev" '(diminish 'abbrev-mode))
   (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
   (eval-after-load "projectile" '(diminish 'projectile-mode "Prj"))))

(defun ecfg--setup-color-theme ()
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" ecfg-dir))
  (setq x-underline-at-descent-line t)
  (load-theme 'ecfg-light t))
