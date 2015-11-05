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
;;; Disabling unused UI components
  (setq-default inhibit-startup-screen t)

  ;; we don't need menubar (execpt OSX), toolbar nor scrollbar
  (and (fboundp 'menu-bar-mode)
       (not (eq system-type 'darwin))
       (menu-bar-mode -1))
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

;;; Scrolling
  ;; scroll two lines at a time (less "jumpy" than defaults but still snappy)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  ;; don't accelerate scrolling
  (setq mouse-wheel-progressive-speed nil)
  ;; scroll window under mouse
  (setq mouse-wheel-follow-mouse 't)
  ;; The number of lines to try scrolling a window by when point moves out.
  ;; Actually I like the default behavior with re-centering of the screen
  ;; (setq scroll-step 1)

;;; Text-related UI
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
